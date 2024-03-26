module Unix = UnixLabels
open Core
open StdLabels

let _create ~stdout ~stdin ~stderr args =
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr

let exec (Cmd.{args; stdin; stdout; stderr} as cmd) =
  let in' = Stream.prep_in stdin in
  let out = Stream.prep_out stdout in
  let err = Stream.prep_out stderr in
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
    let output =
      try f t with
      | e ->
        let _ = t.close () in
        raise e
    in
    t.close (), output
