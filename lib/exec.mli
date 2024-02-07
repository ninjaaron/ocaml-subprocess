open Core

val exec : ('i, 'o, 'e) Cmd.t -> ('i, 'o, 'e) t
val in_context : ('i, 'o, 'e) Cmd.t -> f:(('i, 'o, 'e) t -> 'a) -> Exit.t * 'a
