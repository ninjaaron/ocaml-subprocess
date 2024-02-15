module Unix = UnixLabels

let get_devnull () =
Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY; Unix.O_CLOEXEC] ~perm:0o000 

type 'a bookkeeping =
{ handle: 'a
; send: Unix.file_descr
; cl: Unix.file_descr option
; closer: unit -> unit
}

open Core

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
