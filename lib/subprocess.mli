module Core = Core

include module type of Core

(** The following functions are generated in a functor to provide a
   similar interface across several output types. When the
   documentation speaks about something being "wrapped in the ouptut
   type", it means the [Subprocess_error] is raised in the event of
   non-zero exit status, and otherwise the value is unchanged.  *)

include Functor.S with type 'a t := 'a

(**
   [let&] is a binding operator which wrappes {!exec} to provide a
   cleaner syntax, especially when binding multipe processes in a
   pipeline.

   {[
     let& proc = cmd ["echo"; "foo"] |> pipe_out in
     In_channel.input_all (stdout proc)

     (* equivalent to *)

     exec (cmd ["echo"; "foo"] |> pipe_out)
       ~f:(fun proc -> In_channel.input_all (stdout proc))
   ]} *)
val (let&) : ('stdin, 'stdout, 'stderr) Cmd.t ->
  (('stdin, 'stdout, 'stderr) t -> 'a) -> 'a

module Results : sig

  (** The {!Results} module presents an interface similar to the
      top-level functions in the {!Subprocess} module, but the output
      is wrapped in a result type, where a non-zere exit status
      becomes [Error of] {!Exit.t}.
      {!module-Core} is included here to avoid having to open both
      [Subprocess] and [Results].  *)

  include Functor.S with type 'a t := ('a, Exit.t) result

  (** Similar to {!exec}, but composes better for monadic binding. *)
  val bind : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  (** Similar to {!exec_joined}, but composes better for monadic
      binding. *) 
  val bind_joined : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  (** Converts [Error of] {!Exit.t} to [Error of string] as a more
      general, composable Error type. *)
  val string_error : ('a, Exit.t) result -> ('a, string) result

  (** Just an alias for [Stdlib.Result.bind]. If you like monads, you
      know what to do. *)
  val (let*) : 
    ('a, 'b) result ->
    ('a -> ('c, 'b) result) ->
    ('c, 'b) result

  (** Same as {!bind}, but as a binding operator for monad funtimes. *)
  val (let&) : 
    ('stdin, 'stderr, 'stdout) Cmd.t ->
    (('stdin, 'stderr, 'stdout) t -> ('a, Exit.t) result) ->
    ('a, Exit.t) result

  include module type of Core
end

module StringResults : sig

  (** The {!StringResults} module presents an interface similar to the
      functions in the {!Results} module, but the output is wrapped in
      a result type, where a non-zere exit status becomes [Error of
      string]. [string] is perhaps less useful than [Exit.t] but it
      composes better. {!module-Core} is included here to avoid
      having to open both [Subprocess] and [StringResults].  *)

  include Functor.S with type 'a t := ('a, string) result

  (** Similar to {!exec}, but composes better for monadic binding. *)
  val bind : 
    ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) t -> ('a, string) result) ->
    ('a, string) result

  (** Similar to {!exec_joined}, but composes better for monadic
      binding. *) 
  val bind_joined : 
    ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) t -> ('a, string) result) ->
    ('a, string) result

  (** Just an alias for [Stdlib.Result.bind]. If you like monads, you
      know what to do. *)
  val (let*) : 
    ('a, 'b) result ->
    ('a -> ('c, 'b) result) ->
    ('c, 'b) result

  (** Same as {!bind}, but as a binding operator for monad funtimes. *)
  val (let&) : 
    ('stdin, 'stderr, 'stdout) Cmd.t ->
    (('stdin, 'stderr, 'stdout) t -> ('a, string) result) ->
    ('a, string) result
  include module type of Core
end

module Unchecked : sig

  (** The {!Unchecked} module presents an interface similar to the
      top-level functions in the {!Subprocess} module, but the output
      is wrapped as a pair with [Exit.t] as the first element,
      {!module-Core} is included here to avoid having to open both
      [Subprocess] and [Results].  *)

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
