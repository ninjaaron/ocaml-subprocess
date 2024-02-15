open Core

type 'a bookkeeping =
{ handle: 'a
; send: Unix.file_descr
; cl: Unix.file_descr option
; closer: unit -> unit
}

val get_devnull : unit -> Unix.file_descr
val prep_out : 'a Cmd.Out.t -> 'a Out.t bookkeeping
val prep_in : 'a Cmd.In.t -> 'a In.t bookkeeping
