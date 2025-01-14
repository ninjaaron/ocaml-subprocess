module Unix = UnixLabels
open Core
open StdLabels

module Stream = struct
  let get_devnull () =
    Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY; Unix.O_CLOEXEC] ~perm:0o000 

  type 'a bookkeeping =
    { handle: 'a
    ; send: Unix.file_descr
    ; cl: Unix.file_descr option
    ; closer: unit -> unit
    }

  let prep_fd fd handle =
    { handle
    ; send=fd
    ; cl=None
    ; closer = Fun.id
    }

  let channel_helper descr_of_channel oc =
    let fd = descr_of_channel oc in
    Unix.set_close_on_exec fd;
    fd

  let prep_out (type a) : a Cmd.Out.t -> a Out.t bookkeeping = function
    | Stdout -> prep_fd Unix.stdout Out.Stdout
    | Stderr -> prep_fd Unix.stderr Out.Stderr
    | Devnull -> prep_fd (get_devnull ()) Out.Devnull
    | Channel oc ->
      let fd = channel_helper Unix.descr_of_out_channel oc in
      prep_fd fd Out.Channel
    | File s ->
      let fd = channel_helper Unix.descr_of_out_channel (Out_channel.open_text s) in
      prep_fd fd (Out.File s)
    | Pipe ->
      let r, w = Unix.pipe ~cloexec:true () in
      let ic = Unix.in_channel_of_descr r in
      { handle = Out.Pipe ic
      ; send = w
      ; cl = Some w
      ; closer = fun () -> In_channel.close ic
      }

  let prep_in (type a) : a Cmd.In.t -> a In.t bookkeeping = function
    | Stdin -> prep_fd Unix.stdin In.Stdin
    | Channel ic ->
      let fd = channel_helper Unix.descr_of_in_channel ic in
      prep_fd fd In.Channel
    | File s ->
      let fd = channel_helper Unix.descr_of_in_channel (In_channel.open_text s) in
      prep_fd fd (In.File s)
    | Pipe ->
      let r, w = Unix.pipe ~cloexec:true () in
      let oc = Unix.out_channel_of_descr w in
      { handle = In.Pipe oc
      ; send = r
      ; cl = Some r
      ; closer = fun () -> Out_channel.close oc
      }
end

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
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  { pid
  ; cmd
  ; stdin = in'.handle
  ; stdout = out.handle
  ; stderr = err.handle
  ; close
  }

let in_context cmd ~f =
  let t = exec cmd in
  match f t with
  | output -> t.close (), output
  | exception e ->
    let _ = t.close () in
    raise e

let shared_pipe (Cmd.{args; stdin; _} as cmd) =
  let in', out, err =
    let out = Stream.prep_out Pipe in
    Stream.prep_in stdin, out, out in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = Unix.waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.{pid; status; cmd = (Cmd.to_mono cmd)} in
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl ];
  { pid
  ; cmd={cmd with stdout=Pipe; stderr=Stdout}
  ; stdin = in'.handle
  ; stdout = out.handle
  ; stderr = Stdout
  ; close
  }

let shared_context cmd ~f =
  let t = shared_pipe cmd in
  match f t with
  | output -> t.close (), output
  | exception e ->
    let _ = t.close () in
    raise e
