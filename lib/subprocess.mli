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
      is wrapped as a pair with {!Exit.t} as the first element,
      {!module-Core} is included here to avoid having to open both
      [Subprocess] and [Unchecked].  *)

  include Functor.S with type 'a t = Exit.t * 'a

  (** Execute the command and wait for it to exit, returning an
      instance of {!Exit.t} *)
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> Exit.t


  (** Execute the command and write the [input] string to the stdin of
      the process.  *)
  val write : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string -> Exit.t

  (** Execute the command and write the [input] [Seq.t] instance to
      the stdin of the process, separated by newline characters. If
      you want to read from stdout while writing, use
      {!fold_with}.  *)
  val write_lines : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string Seq.t -> Exit.t

  (** the [Unchecked] module does not include a binding operator
      because I have't yet found a way to get it to compose nicely. *)

  include module type of Core
end

(** The {!Exec} module is exposed for starting processes without a
    managing scope. {!Exec.exec} and {!Exec.shared_pipe} exist for
    this purpose. *)
module Exec = Exec

(** The following functions expect that their inputs process handles
    were executed with a {!Cmd.t} instance where [block=false] and will
    raise this exception if it was not. We could, in principle, prevent
    this at a type level, but non-blocking I/O operations were added
    quite late in the design of this library, and I'm still debating
    whether adding yet another type parameter is wise. For the time
    being, it's an exception. *)
exception Non_blocking_io_expected of string

(** Takes a process handle where both stdout and stderr are set to pipe
    as input and reads streams from each into a string, returning the
    pair of strings. Raises {!Non_blocking_io_expected} if I/O is
    blocking. *)
val read_both_proc : ?sleep:float -> ('stdin, pipe, pipe) t -> (string * string)

(** Takes a process handle where both stdout and stderr are set to pipe
    and folds over each. See {!fold_both} for more usage details.
    Raises {!Non_blocking_io_expected} if I/O is blocking. *)
val fold_both_proc : ?sleep:float ->
  ('stdin, pipe, pipe) t ->
  f:('acc -> (string, string) result -> 'acc) ->
  init:'acc ->
  'acc

(** Takes a process handle where both stdin and stdout are set to pipe
    and folds over output. See {!fold_with} for more usage details.
    Raises {!Non_blocking_io_expected} if I/O is blocking. *)
val fold_with_proc : ?sleep:float ->
  ?sep:string ->
  (pipe, pipe, 'stderr) t ->
  lines:string Seq.t ->
  f:('acc -> string -> 'acc) ->
  init:'acc ->
  'acc
