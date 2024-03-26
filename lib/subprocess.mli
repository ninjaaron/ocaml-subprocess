module Core = Core
include module type of Core

val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
  f:(('stdin, 'stdout, 'stderr) t -> 'a) ->
  'a

val run : ('stdin, 'stdout, 'stderr) Cmd.t -> unit
val read : ('stdin, stdout, 'stderr) Cmd.t -> string
val lines : ('stdin, stdout, 'stderr) Cmd.t -> string list
val read_err : ('stdin, 'stdout, stderr) Cmd.t -> string
val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> string list
val fold :
  ('stdin, stdout, 'stderr) Cmd.t ->
  f:('acc -> string -> 'acc) ->
  init:'acc ->
  'acc
val fold_err :
  ('stdin, 'stdout, stderr) Cmd.t ->
  f:('acc -> string -> 'acc) ->
  init:'acc ->
  'acc

module Exit = Exit

module Results : sig
  val exec : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> (unit, Exit.t) result
  val read : ('stdin, stdout, 'stderr) Cmd.t -> (string, Exit.t) result
  val lines : ('stdin, stdout, 'stderr) Cmd.t -> (string list, Exit.t) result
  val read_err : ('stdin, 'stdout, stderr) Cmd.t -> (string, Exit.t) result
  val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> (string list, Exit.t) result
  val fold : 
    ('stdin, stdout, 'stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    ('acc, Exit.t) result
  val fold_err : 
    ('stdin, 'stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    ('acc, Exit.t) result

  val (let*) : 
    ('a, 'b) result ->
    ('a -> ('c, 'b) result) ->
    ('c, 'b) result
  val (let|) : 
    ('stdin, 'stderr, 'stdout) Cmd.t ->
    (('stdin, 'stderr, 'stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  include module type of Core
end

module Unchecked : sig
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> 'a) ->
    Exit.t * 'a
  val run :
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    ('stdout, 'stderr) Run.t
  val read :
    ('stdin, stdout, 'stderr) Cmd.t ->
    (string, 'stderr) Run.t
  val lines :
    ('stdin, stdout, 'stderr) Cmd.t ->
    (string list, 'stderr) Run.t
  val read_err :
    ('stdin, 'stdout, stderr) Cmd.t ->
    ('stdout, string) Run.t
  val lines_err :
    ('stdin, 'stdout, stderr) Cmd.t ->
    ('stdout, string list) Run.t

  include module type of Core
end

module Exec = Exec

