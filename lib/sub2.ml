open Base
include Core

let wait ?(mode = []) t = Unix.waitpid ~mode t.pid
let poll t =
  match Unix.waitpid ~mode:[ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, status -> Some status

let create ~stdin ~stdout ~stderr args =
  let in' = Stream2.prep_in stdin in
  let out = Stream2.prep_out stdout in
  let err = Stream2.prep_out stderr in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = Unix.waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.{pid; status; args} in
  let t = { pid
          ; args
          ; stdin = in'.handle
          ; stdout = out.handle
          ; stderr = err.handle
          ; close
          } in
  List.iter ~f:(Option.iter ~f:Unix.close) [ in'.cl; out.cl; err.cl ];
  t

let open_in ?(stdout=Out.Fd Unix.stdout) ?(stderr=Out.Fd Unix.stderr) args =
  create ~stdin:Pipe ~stdout ~stderr args

let open_out ?(stdin=In.Fd Unix.stderr) ?(stderr=Out.Fd Unix.stderr) args =
  create ~stdin ~stdout:Pipe ~stderr args

let open_err ?(stdin=In.Fd Unix.stderr) ?(stdout=Out.Fd Unix.stderr) args =
  create ~stdin ~stdout ~stderr:Pipe args

let exec
    ?(stdin=In.Fd Unix.stderr)
    ?(stdout=Out.Fd Unix.stderr)
    ?(stderr=Out.Fd Unix.stderr)
    args
  = create ~stdin ~stderr ~stdout args

let check t =
  Exit.check (t.close ())

let in_context t ~f =
    let output =
      try f t with
      | e ->
        let _ = t.close () in
        raise e
    in
    t.close (), output

let (>>=) t f =
  let proc, output = in_context t ~f in
  Result.bind ~f:(fun _ -> output) (Exit.check proc)
  
let (let*) = (>>=)

let or_error res = Result.map_error res
    ~f:(fun err -> Error.create "bad status" err Exit.sexp_of_t)
let string_error res = Result.map_error res
    ~f:Exit.to_string

