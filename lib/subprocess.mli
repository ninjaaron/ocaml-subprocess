module Core = Core

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

module StringResults : sig
  include Functor.S with type 'a t := ('a, string) result

  val bind : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, string) result) ->
    ('a, string) result
  val bind_joined : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> ('a, string) result) ->
    ('a, string) result

  val (let*) : 
    ('a, 'b) result ->
    ('a -> ('c, 'b) result) ->
    ('c, 'b) result
  val (let&) : 
    ('stdin, 'stderr, 'stdout) Cmd.t ->
    (('stdin, 'stderr, 'stdout) t -> ('a, string) result) ->
    ('a, string) result
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

val read_both_proc : ('stdin, pipe, pipe) t -> (string * string)

val fold_both_proc : ?sleep:float ->
  ('stdin, pipe, pipe) t ->
  f:('acc -> (string, string) result -> 'acc) ->
  init:'acc ->
  'acc

val fold_with_proc : ?sep:string ->
  (pipe, pipe, 'stderr) t ->
  lines:string Seq.t ->
  f:('acc -> string -> 'acc) ->
  init:'acc ->
  'acc

module Exec = Exec
