module Core = Core
module Cmd = Cmd

include module type of Core
include Functor.S with type 'a t := 'a
val (let&) : ('stdin, 'stdout, 'stderr) Cmd.t ->
  (('stdin, 'stdout, 'stderr) t -> 'a) -> 'a
  
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

  include module type of Core
end

module Unchecked : sig
  include Functor.S with type 'a t = Exit.t * 'a
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> Exit.t
  include module type of Core
end

module Exec = Exec
