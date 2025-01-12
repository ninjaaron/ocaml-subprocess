type t =
  { pid : int
  ; cmd : Cmd.Mono.t
  ; status : Unix.process_status
  }

val pp : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
val show : t -> string
val res : t * 'a -> ('a, t) result
val check : t -> (t, t) result
val string_error : ('a, t) result -> ('a, string) result
val exn : t * 'a -> 'a
val status_to_string : Unix.process_status -> string
val unify_status : Unix.process_status -> string * int
