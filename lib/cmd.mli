open Io
module In : sig
  type _ t =
      Stdin : stdin t
    | Channel : in_channel -> channel t
    | File : string -> file t
    | Pipe : pipe t
  val to_mono : 'a t -> Mono.t
  val show : 'a t -> string
end

module Out : sig
  type _ t =
      Stdout : stdout t
    | Stderr : stderr t
    | Channel : out_channel -> channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : pipe t
  val to_mono : 'a t -> Mono.t
  val show : 'a t -> string
end

type ('stdin, 'stdout, 'stderr) t =
  { args : string array
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; env : string array
  }

module Mono : sig
  type t 
  val pp : Format.formatter -> t -> unit
end

val arg_to_repr : string -> string
val pp_args : Format.formatter -> string array -> unit
val to_mono : ('a, 'b, 'c) t -> Mono.t
val pp : Format.formatter -> ('a, 'b, 'c) t -> unit
[@@ocaml.toplevel_printer]
val show : ('a, 'b, 'c) t -> string

