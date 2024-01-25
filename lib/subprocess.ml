module Unix = UnixLabels

exception Subprocess_error of string

let get_devnull () =
  Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY; Unix.O_CLOEXEC] ~perm:0o000 

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then failwith "process arguments can't be empty";
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr
;;

type 'a _stream_bookkeeping =
  { handle: 'a option
  ; send: Unix.file_descr
  ; cl: Unix.file_descr option
  }

let prep_fd fd = {handle=None; send=fd; cl=None}
                 
let rec prep_out_stream = function
  | `Devnull -> prep_fd (get_devnull ())
  | `Fd fd -> prep_fd fd
  | `Out_channel oc ->
    let fd = Unix.descr_of_out_channel oc in
    Unix.set_close_on_exec fd;
    prep_fd fd
  | `Pipe ->
    let r, w = Unix.pipe ~cloexec:true () in
    { handle = Some (Unix.in_channel_of_descr r)
    ; send = w
    ; cl = Some w
    }
;;

let prep_in_stream = function
  | `Fd fd -> prep_fd fd
  | `In_channel ic ->
    let fd = Unix.descr_of_in_channel ic in
    Unix.set_close_on_exec fd;
    prep_fd fd
  | `Pipe ->
    let r, w = Unix.pipe ~cloexec:true () in
    { handle = Some (Unix.out_channel_of_descr w)
    ; send = r
    ; cl = Some r
    }
;;

type t =
  { pid : int
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
type completed =
  { pid : int
  ; status : Unix.process_status
  }


let close t =
  Option.iter close_out t.stdin;
  let pid, status = wait t in
  List.iter (Option.iter close_in) [t.stdout; t.stderr];
  {pid; status}

type input_t =
  [ `Fd of Unix.file_descr
  | `In_channel of in_channel
  | `Pipe
  ]

type output_t =
  [ `Fd of Unix.file_descr
  | `Out_channel of out_channel
  | `Pipe
  | `Devnull
  ]

let create
    ?(stdin = `Fd Unix.stdin)
    ?(stdout = `Fd Unix.stdout)
    ?(stderr = `Fd Unix.stderr)
    args
  =
  let in' = prep_in_stream stdin in
  let out = prep_out_stream stdout in
  let err = prep_out_stream stderr in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let t = { pid
          ; stdin = in'.handle
          ; stdout = out.handle
          ; stderr = err.handle
          } in
  List.iter (Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  t

let check completed =
  match completed.status with
  | Unix.WEXITED 0 -> Ok completed
  | _ -> Error "process exited non-zero"
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

  let create ?stdin ?stdout ?stderr args ~f =
    let t = create ?stdin ?stdout ?stderr args in
    from t ~f
  ;;
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

type run_in =
  [ `Fd of Unix.file_descr
  | `In_channel of in_channel
  ]

type run_t =
  { proc: completed
  ; stdout: string list
  ; stderr: string list
  }

let _unwrap_run (proc, (stdout, stderr)) = {proc; stdout; stderr}

let run ?stdin ?stdout ?stderr args =
  Context.create ?stdin ?stdout ?stderr args
    ~f:(fun t -> List.of_seq (lines t),
                 List.of_seq (stderr_lines t))
  |> _unwrap_run
;;

let run_check ?stdin ?stdout ?stderr args =
let run_t = run ?stdin ?stdout ?stderr args in
  Result.map (fun _ -> run_t) (check run_t.proc)
  

let write ?stdout ?stderr args ~input =
  Context.create ~stdin:`Pipe ?stdout ?stderr args
    ~f:(fun t ->
        write_stdin t input;
        List.of_seq (lines t), List.of_seq (stderr_lines t))
  |> _unwrap_run
;;

let fold ?stdin ?stdout ?stderr args ~f ~init =
  let reduce_from reader = Context.create ?stdin ?stdout ?stderr args
      ~f:(fun proc -> Seq.fold_left f init (reader proc)) in
  match stdout, stderr with
    Some `Pipe, Some `Pipe ->
    raise (Subprocess_error "cannot fold from two pipes.")
  | Some `Pipe, _ -> reduce_from lines
  | _, Some `Pipe -> reduce_from stderr_lines
  | _, _ -> raise (Subprocess_error "fold: either stdout or stderr must be set to `Pipe")
;;
