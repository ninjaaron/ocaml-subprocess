module Unix = UnixLabels

let get_devnull () =
Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY; Unix.O_CLOEXEC] ~perm:0o000 

type 'a bookkeeping =
{ handle: 'a option
; send: Unix.file_descr
; cl: Unix.file_descr option
}

let prep_fd fd = {handle=None; send=fd; cl=None}

let prep_out default = function
| None -> prep_fd default
| Some `Devnull -> prep_fd (get_devnull ())
| Some (`Out_channel oc) ->
    let fd = Unix.descr_of_out_channel oc in
    Unix.set_close_on_exec fd;
    prep_fd fd
| Some `Pipe ->
    let r, w = Unix.pipe ~cloexec:true () in
    { handle = Some (Unix.in_channel_of_descr r)
    ; send = w
    ; cl = Some w
    }

let prep_in default = function
| None -> prep_fd default
| Some (`In_channel ic) ->
    let fd = Unix.descr_of_in_channel ic in
    Unix.set_close_on_exec fd;
    prep_fd fd
| Some (`Pipe) ->
    let r, w = Unix.pipe ~cloexec:true () in
    { handle = Some (Unix.out_channel_of_descr w)
    ; send = r
    ; cl = Some r
    }
