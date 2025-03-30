module Core = Core
module Managed_in = Managed_in

include module type of Core

val open_out : ('stdin, stdout, 'stderr) Cmd.t -> Managed_in.t
val open_err : ('stdin, 'stdout, stderr) Cmd.t -> Managed_in.t

include Functor.S with type 'a t := 'a

val (let&) : ('stdin, 'stdout, 'stderr) Cmd.t ->
  (('stdin, 'stdout, 'stderr) t -> 'a) -> 'a

val managed_read : Managed_in.t -> string
val managed_lines : Managed_in.t -> string list
val managed_line : Managed_in.t -> string option
val managed_char : Managed_in.t -> char option
val managed_byte : Managed_in.t -> int option
val managed_input :
  Managed_in.t -> buf:bytes -> pos:int -> len: int ->  int
  
module Results : sig
  include Functor.S with type 'a t := ('a, Exit.t) result

  val bind : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result
  val bind_joined : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  val string_error : ('a, Exit.t) result -> ('a, string) result

  val (let*) : 
    ('a, 'b) result ->
    ('a -> ('c, 'b) result) ->
    ('c, 'b) result
  val (let&) : 
    ('stdin, 'stderr, 'stdout) Cmd.t ->
    (('stdin, 'stderr, 'stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  val managed_read : Managed_in.t -> (string, Exit.t) result
  val managed_lines : Managed_in.t -> (string list, Exit.t) result

  include module type of Core
end

module Unchecked : sig
  include Functor.S with type 'a t = Exit.t * 'a
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> Exit.t
  val write : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string -> Exit.t
  val write_lines : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string Seq.t -> Exit.t
  include module type of Core
end

module Exec = Exec
