type t = { pid : int; args : string array; status : Unix.process_status; }
val to_string : t -> string
val check : t -> (t, t) result
val string_error : ('a, t) result -> ('a, string) result
val exn : ('a, t) result -> 'a
val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
