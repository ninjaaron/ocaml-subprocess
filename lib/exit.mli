type t =
  { pid : int
  ; cmd : Cmd.Mono.t
  ; status : Unix.process_status
  }

val pp : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
val show : t -> string
val check : t -> (t, t) result
val string_error : ('a, t) result -> ('a, string) result
val exn : ('a, t) result -> 'a
val status_to_string : Unix.process_status -> string
