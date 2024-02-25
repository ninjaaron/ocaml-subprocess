type t = List of t list | Atom of string
val show : t -> string
val pp : Format.formatter -> t -> unit
[@@ocaml.toplevel_printer]
val atom_exn : t -> string
val list_exn : t -> t list
val mkatom : ('a -> string) -> 'a -> t
val unwrap_atom : (string -> 'a) -> t -> 'a

module Of : sig
  val parse : string -> t
  val list : f:('a -> t) -> 'a list -> t
  val array : f:('a -> t) -> 'a array -> t
  val field : (string * 'a) -> ('a -> t) -> t
  val int : int -> t
  val float : float -> t
  val bool : bool -> t
  val char : char -> t
  val unit : unit -> t
  val string : string -> t
end

module To : sig
  val list : f:(t -> 'a) -> t -> 'a list
  val array : f:(t -> 'a) -> t -> 'a array
  val field : string -> t -> (t -> 'a) -> 'a
  val int : t -> int
  val float : t -> float
  val bool : t -> bool
  val char : t -> char
  val unit : t -> unit
  val string : t -> string
end
