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

let prep_out (type a) : a Out.t -> a bookkeeping = function
  | Stdout -> prep_fd Unix.stdout Stdout
  | Stderr -> prep_fd Unix.stderr Stderr
  | Devnull -> prep_fd (get_devnull ()) Devnull
  | Channel oc ->
    let fd = channel_helper Unix.descr_of_out_channel oc in
    prep_fd fd Channel
  | File s ->
    let fd = channel_helper Unix.descr_of_out_channel (Out_channel.open_text s) in
    prep_fd fd (File s)
  | Pipe ->
    let r, w = Unix.pipe ~cloexec:true () in
    let handle = Unix.in_channel_of_descr r in
    { handle
    ; send = w
    ; cl = Some w
    ; closer = fun () -> In_channel.close handle
    }

let prep_in (type a) : a In.t -> a bookkeeping = function
  | Stdin -> prep_fd Unix.stdin Stdin
  | Channel ic ->
    let fd = channel_helper Unix.descr_of_in_channel ic in
    prep_fd fd Channel
  | File s ->
    let fd = channel_helper Unix.descr_of_in_channel (In_channel.open_text s) in
    prep_fd fd (File s)
  | Pipe ->
    let r, w = Unix.pipe ~cloexec:true () in
    let handle = Unix.out_channel_of_descr w in
    { handle
    ; send = r
    ; cl = Some r
    ; closer = fun () -> Out_channel.close handle
    }
