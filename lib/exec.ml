module Unix = UnixLabels
open Core
open StdLabels

let _create ~stdout ~stdin ~stderr args =
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr

let exec (Cmd.{args; stdin; stdout; stderr} as cmd) =
  let in', out, err =
    Stream.(prep_in stdin, prep_out stdout, prep_out stderr) in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = Unix.waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.{pid; status; cmd = (Cmd.to_mono cmd)} in
  let t = { pid
          ; cmd
          ; stdin = in'.handle
          ; stdout = out.handle
          ; stderr = err.handle
          ; close
          } in
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  t

let in_context cmd ~f =
  let t = exec cmd in
  match f t with
  | output -> t.close (), output
  | exception e ->
    let _ = t.close () in
    raise e
