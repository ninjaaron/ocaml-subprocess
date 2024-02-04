type t = { pid : int; args : string array; status : Unix.process_status; }
val sexp_of_t : t -> Base.Sexp.t
val t_of_sexp : Base.Sexp.t -> t
val to_string : t -> string
val check : t -> (t, t) result
