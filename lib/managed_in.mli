open Core

type t = Pipe : { proc: ('a, 'b, 'c) Core.t ; ic: In_channel.t} -> t

val close : t -> Exit.t
val char : t -> (Exit.t, char) Either.t
val line : t -> (Exit.t, string) Either.t
val byte : t -> (Exit.t, int) Either.t
val all : t -> Exit.t * string
val lines : t -> Exit.t * string list
val input : t -> buf:bytes -> pos:int -> len:int -> (Exit.t, int) Either.t
