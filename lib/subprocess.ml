module Unix = UnixLabels

module Exit = Exit

exception Subprocess_error of string

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then
    raise (Subprocess_error "process arguments can't be empty");
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr
;;

type t =
  { pid : int
  ; args : string array
  ; stdin : out_channel option
  ; stdout : in_channel option
  ; stderr : in_channel option
  }

let wait ?(mode = []) t = Unix.waitpid ~mode t.pid

let poll t =
  match Unix.waitpid ~mode:[ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, status -> Some status
;;

let close t =
  Option.iter close_out t.stdin;
  let pid, status = wait t in
  List.iter (Option.iter close_in) [t.stdout; t.stderr];
  Exit.{pid; status; args = t.args}

let check t =
  Exit.check (close t)

type input_t =
  [ `In_channel of in_channel
  | `Pipe
  ]

type output_t =
  [ `Out_channel of out_channel
  | `Pipe
  | `Devnull
  ]

let create ?stdin ?stdout ?stderr args =
  let in' = Stream.prep_in Unix.stdin stdin in
  let out = Stream.prep_out Unix.stdout stdout in
  let err = Stream.prep_out Unix.stderr stderr in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let t = { pid
          ; args
          ; stdin = in'.handle
          ; stdout = out.handle
          ; stderr = err.handle
          } in
  List.iter (Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  t
;;

module Context = struct
  let from t ~f =
    let output =
      try f t with
      | e ->
        let _ = close t in
        raise e
    in
    close t, output
  ;;

  let unchecked ?stdin ?stdout ?stderr args ~f =
    let t = create ?stdin ?stdout ?stderr args in
    from t ~f
  ;;

  let create ?stdin ?stdout ?stderr args ~f =
    let proc, out = unchecked ?stdin ?stdout ?stderr args ~f in
    Result.map (fun _ -> out) (Exit.check proc)
    
end

let write_stdin t string =
  output_string (Option.get t.stdin) string
;;

let opt_line icopt =
  try Option.map input_line icopt with
  | End_of_file -> None
;;

let rec seq_of_ic ic () =
  try Seq.Cons (input_line ic, seq_of_ic ic) with
  | End_of_file -> Seq.Nil
;;

let opt_lines icopt = Option.fold ~none:Seq.empty ~some:seq_of_ic icopt
let line t = opt_line t.stdout
let stderr_line t = opt_line t.stderr
let lines t = opt_lines t.stdout
let stderr_lines t = opt_lines t.stderr

module Run = struct
  type t =
    { proc: Exit.t
    ; stdout: string list
    ; stderr: string list
    }
  let unwrap (proc, (stdout, stderr)) =
    {proc; stdout; stderr}

  let unchecked ?(stdin=Stdlib.stdin) ?stdout ?stderr args =
    Context.unchecked ~stdin:(`In_channel stdin) ?stdout ?stderr args
      ~f:(fun t -> List.of_seq (lines t),
                   List.of_seq (stderr_lines t))
    |> unwrap 

  let run ?stdin ?stdout ?stderr args =
    let t = unchecked ?stdin ?stdout ?stderr args in
    Result.map (fun _ -> t) (Exit.check t.proc)

  let filter ?stdout ?stderr args ~input =
    Context.unchecked ~stdin:`Pipe ?stdout ?stderr args
      ~f:(fun t ->
          write_stdin t input;
          Option.get t.stdin |> close_out;
          List.of_seq (lines t), List.of_seq (stderr_lines t))
    |> unwrap
end

let run = Run.run


;;

let fold ?(stdin=Stdlib.stdin) ?(stderr=false) args ~f ~init =
   let partial, reader = if stderr
      then Context.unchecked ~stdin:(`In_channel stdin) ~stderr:`Pipe args, stderr_lines
      else Context.unchecked ~stdin:(`In_channel stdin) ~stdout:`Pipe args, lines in
    partial ~f:(fun proc -> Seq.fold_left f init (reader proc))
;;
