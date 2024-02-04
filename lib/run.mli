type 'out t = { proc : Exit.t; stdout : 'out; stderr : 'out; }

module Read : sig
  val unchecked : string array -> string t
  val res : string array -> (string, Exit.t) result
  val exn : string array -> string
  val unchecked_err : string array -> string t
  val res_err : string array -> (string, Exit.t) result
  val exn_err : string array -> string
  val unchecked_both : string array -> string t
  val res_both : string array -> (string t, Exit.t) result
  val exn_both : string array -> string t
end

module Lines : sig
  val unchecked : string array -> string list t
  val res : string array -> (string list, Exit.t) result
  val exn : string array -> string list
  val unchecked_err : string array -> string list t
  val res_err : string array -> (string list, Exit.t) result
  val exn_err : string array -> string list
  val unchecked_both : string array -> string list t
  val res_both : string array -> (string list t, Exit.t) result
  val exn_both : string array -> string list t
end

val unchecked : string array -> unit t
val res : string array -> (unit, Exit.t) result
val exn : string array -> unit
val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
