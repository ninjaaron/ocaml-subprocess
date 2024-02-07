module Unix = UnixLabels
open Base
open Core

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then
    raise (Subprocess_error "process arguments can't be empty");
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr

let exec Cmd.{args; stdin; stdout; stderr} =
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

let in_context cmd ~f =
  let t = exec cmd in
    let output =
      try f t with
      | e ->
        let _ = t.close () in
        raise e
    in
    t.close (), output
