module Core = Core
module Cmd = Cmd

include module type of Core

val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
  f:(('stdin, 'stdout, 'stderr) t -> 'a) -> 'a
val exec_both : ('stdin, stdout, stderr) Cmd.t ->
  f:(('stdin, pipe, stdout) t -> 'a) -> 'a

val run : ('stdin, 'stdout, 'stderr) Cmd.t -> unit
val read : ('stdin, stdout, 'stderr) Cmd.t -> string
val lines : ('stdin, stdout, 'stderr) Cmd.t -> string list
val read_err : ('stdin, 'stdout, stderr) Cmd.t -> string
val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> string list
val read_both : ('stdin, stdout, stderr) Cmd.t -> string
val lines_both : ('stdin, stdout, stderr) Cmd.t -> string list
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
val fold_both :
  ('stdin, stdout, stderr) Cmd.t ->
  f:('acc -> string -> 'acc) ->
  init:'acc ->
  'acc

val (let&) : ('stdin, 'stdout, 'stderr) Cmd.t ->
  (('stdin, 'stdout, 'stderr) t -> 'a) -> 'a
  

module Exit = Exit

module Results : sig
  val exec : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result
  val exec_both : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> (unit, Exit.t) result
  val read : ('stdin, stdout, 'stderr) Cmd.t -> (string, Exit.t) result
  val lines : ('stdin, stdout, 'stderr) Cmd.t -> (string list, Exit.t) result
  val read_err : ('stdin, 'stdout, stderr) Cmd.t -> (string, Exit.t) result
  val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> (string list, Exit.t) result
  val read_both : ('stdin, stdout, stderr) Cmd.t -> (string, Exit.t) result
  val lines_both : ('stdin, stdout, stderr) Cmd.t -> (string list, Exit.t) result
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
  val fold_both : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    ('acc, Exit.t) result

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
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> 'a) ->
    Exit.t * 'a
  val exec_both : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> 'a) ->
    Exit.t * 'a
  val run :
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    Exit.t
  val read :
    ('stdin, stdout, 'stderr) Cmd.t ->
    Exit.t * string
  val lines :
    ('stdin, stdout, 'stderr) Cmd.t ->
    Exit.t * string list
  val read_err :
    ('stdin, 'stdout, stderr) Cmd.t ->
    Exit.t * string
  val lines_err :
    ('stdin, 'stdout, stderr) Cmd.t ->
    Exit.t * string list
  val read_both :
    ('stdin, stdout, stderr) Cmd.t ->
    Exit.t * string
  val lines_both :
    ('stdin, stdout, stderr) Cmd.t ->
    Exit.t * string list
  val fold :
    ('stdin, stdout, 'stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    Exit.t * 'acc
  val fold_err :
    ('stdin, 'stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    Exit.t * 'acc
  val fold_both :
    ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    Exit.t * 'acc

  include module type of Core
end

module Exec = Exec

