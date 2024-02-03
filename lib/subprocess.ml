module Unix = UnixLabels
module Exit = Exit

open Base
open Stdio

exception Subprocess_error of string

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then
    raise (Subprocess_error "process arguments can't be empty");
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr
;;

type t =
  { pid : int
  ; args : string array
  ; stdin : Out_channel.t option
  ; stdout : In_channel.t option
  ; stderr : In_channel.t option
  }

let wait ?(mode = []) t = Unix.waitpid ~mode t.pid

let poll t =
  match Unix.waitpid ~mode:[ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, status -> Some status
;;

let close t =
  Option.iter ~f:Out_channel.close t.stdin;
  let pid, status = wait t in
  List.iter ~f:(Option.iter ~f:In_channel.close) [t.stdout; t.stderr];
  Exit.{pid; status; args = t.args}

let check t =
  Exit.check (close t)

type input_t =
  [ `In_channel of In_channel.t
  | `Pipe
  ]

type output_t =
  [ `Out_channel of Out_channel.t
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
  List.iter ~f:(Option.iter ~f:Unix.close) [ in'.cl; out.cl; err.cl ];
  t

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
    Result.map ~f:(fun _ -> out) (Exit.check proc)
end

let (let*) t f =
  let proc, output = Context.from t ~f in
  Result.bind ~f:(fun _ -> output) (Exit.check proc)

let write_stdin t string =
  Out_channel.output_string (Option.value_exn t.stdin) string
;;

let opt_line icopt =
  Option.bind ~f:In_channel.input_line icopt

let rec seq_of_ic ic () =
  match In_channel.input_line ic with
  | Some line -> Stdlib.Seq.Cons (line, seq_of_ic ic)
  | None -> Stdlib.Seq.Nil
;;

let opt_lines icopt =
  Stdlib.Option.fold ~none:Stdlib.Seq.empty ~some:seq_of_ic icopt
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
      ~f:(fun t -> Stdlib.List.of_seq (lines t),
                   Stdlib.List.of_seq (stderr_lines t))
    |> unwrap 

  let run ?stdin ?stdout ?stderr args =
    let t = unchecked ?stdin ?stdout ?stderr args in
    Result.map ~f:(fun _ -> t) (Exit.check t.proc)

  let filter ?stdout ?stderr args ~input =
    Context.unchecked ~stdin:`Pipe ?stdout ?stderr args
      ~f:(fun t ->
          write_stdin t input;
          Option.value_exn t.stdin |> Out_channel.close;
          Stdlib.(List.of_seq (lines t), List.of_seq (stderr_lines t)))
    |> unwrap
end

let run = Run.run

let fold ?(stdin=Stdlib.stdin) ?(stderr=false) args ~f ~init =
   let partial, reader = if stderr
      then Context.unchecked ~stdin:(`In_channel stdin) ~stderr:`Pipe args, stderr_lines
      else Context.unchecked ~stdin:(`In_channel stdin) ~stdout:`Pipe args, lines in
    partial ~f:(fun proc -> Stdlib.Seq.fold_left f init (reader proc))
;;

let or_error res = Result.map_error res
    ~f:(fun err -> Error.create "bad status" err Exit.sexp_of_t)
let string_error res = Result.map_error res
    ~f:Exit.to_string
