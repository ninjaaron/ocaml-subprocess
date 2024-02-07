open Core

val map_or_error : ('a, Exit.t) result -> ('a, Base.Error.t) result
val map_string_error : ('a, Exit.t) result -> ('a, string) result

val exit_t : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val or_error : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, Base.Error.t) result)
  -> ('a, Base.Error.t) result

val string_error : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, string) result)
  -> ('a, string) result

val exn : ('i, 'o, 'e) Cmd.t -> f:(('i, 'o, 'e) t -> 'a) -> 'a
