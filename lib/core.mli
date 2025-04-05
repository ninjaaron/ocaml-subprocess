(** Here follows the interface defined in the [Core] module.
    This module is included in most of the user-facing modules for
    convenience, so you may stumble across its documentation in
    several places.
    
    The interface defined here is needed to do almost anything with
    the library.
    
    We begin with a set of placeholder types which are used to
    represent handles to the i/o streams of running processes
    (namely, stdin, stdout and stderr). Since only streams that have
    pipes in the parent process are really interactive in any way,
    these non-interactive place holders are used in other
    cases. Their main function is to provide type-level information
    about the streams. *)

type stdin
type stdout
type stderr
type channel
type devnull
type file
type append
type pipe

(** When the library raises, it always raises a Subprocess_error of
    string.  *)
exception Subprocess_error of string


(** home of Cmd.t, which is kind of the whole basis of the thing. *)
module Cmd : sig

  (** In and Out types are mostly just for looking at, not for
      touching, but they don't bite. *)
      
  module In : sig
    type _ t =
        Stdin : stdin t
      | Channel : in_channel -> channel t
      | File : string -> file t
      | Pipe : pipe t
    val show : 'a t -> string
  end

  module Out : sig
    type _ t =
        Stdout : stdout t
      | Stderr : stderr t
      | Channel : out_channel -> channel t
      | File : string -> file t
      | Append : string -> append t
      | Devnull : devnull t
      | Pipe : pipe t
    val show : 'a t -> string
  end

  (** The guts of the Cmd.t in all its glory. You aren't really
      supposed to touch it, but I didn't make it private, so go ahead
      and poke at it. I dare you. I'll laugh my ass off when I change
      it in the next version. *)
  type ('stdin, 'stdout, 'stderr) t =
    { args : string * string array
    ; stdin : 'stdin In.t
    ; stdout : 'stdout Out.t
    ; stderr : 'stderr Out.t
    ; env : string array
    ; block : bool
    }

  (** It's great to have lots of type constraints in this library, but
      it becomes a pain for monadic binding if the exit type has so
      many type parameters, so poly is a wrapper to hide all those
      nasty type variables in Exit.t. *)
  type poly = Poly : ('a, 'b, 'c) t -> poly

  (** obligatory pretty printer, for your debugging pleasure. *)
  val pp : Format.formatter -> ('a, 'b, 'c) t -> unit
  [@@ocaml.toplevel_printer]
  val show : ('a, 'b, 'c) t -> string

end

(** types for exit status with lots of extra info for fun and profit. *)
module Exit : sig

  (** including the Unix status type here so you don't have to import
      it extra. *)
  type status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  (** This is a type you have my full permission to dig around in. I
      included the pid in the exit status and I will tell you why: I
      don't know.

      Still, could be useful if you're registering PIDs in a map or
      something and you want to act on items in the map based on the
      exit status. I don't know. Anyway, it's there if you want it. *)
  type t =
    { pid : int
    ; cmd : Cmd.poly
    ; status : status
    }

  (** Obligatory pretty printer, for your debugging pleasure. *)
  val pp : Format.formatter -> t -> unit
  [@@ocaml.toplevel_printer]
  val show : t -> string

  (** A function to turn a non-zero exit status into an [Error Exit.t]
      and zero into an [Ok 'a] value. [Exit.t * 'a] happens to be the
      return type of {!Exec.in_context}, so this is a helper function
      to convert that type to a result. *)
  val res : t * 'a -> ('a, t) result

  (** Same as above, only it raises {!Subprocess_error} on non-zero. *)
  val exn : t * 'a -> 'a

  (** Helper function to convert your Exit.t monads into string
      monads so they compose a little better. *)
  val string_error : ('a, t) result -> ('a, string) result

  (** Convert the exit status into an integer. *)
  val status_int : t -> int
end

(** In and Out are I/O wrappers for process streams. It's thanks to
    these bad boys we can have such verbose types. *)

module In : sig
  type _ t =
    | Stdin : stdin t
    | Channel : channel t
    | File : string -> file t
    | Pipe : Out_channel.t -> pipe t
end

module Out : sig
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : channel t
    | File : string -> file t
    | Append : string -> append t
    | Devnull : devnull t
    | Pipe : In_channel.t -> pipe t
end


(** The type {!type:t} represents a process which may still be
    running. Some of the higher-level functions don't expose it
    directly, but almost everything else is implemented in terms of
    this type. *)
type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; cmd : ('stdin, 'stdout, 'stderr) Cmd.t
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

(** Obligatory pretty printer, for your debugging pleasure. *)
val pp : Format.formatter -> ('stdin, 'stdout, 'stderr) t -> unit
[@@ocaml.toplevel_printer]
val show : ('stdin, 'stdout, 'stderr) t -> string

(** What follows are some helper functions which fetch pipes to a
    running process for interactive reading and writting. They shadow
    channels with the same names in the standard library. Probably a
    good reason to only open [Subprocess] within a limited scope.

    If you try to use one of these functions to access a stream which
    isn't a pipe, it's a type error. *)

val stdin : (pipe, 'stdout, 'stderr) t -> out_channel
val stdout : ('stdin, pipe, 'stderr) t -> in_channel
val stderr : ('stdin, 'stdout, pipe) t -> in_channel

(** Teeny, tiny, leaky wrapper for [Unix.waitpid] refer to its
    documentation  *)
val wait : ?mode:Unix.wait_flag list
  -> ('stdin, 'stdout, 'stderr) t
  -> int * Exit.status

(** Find out if your process has finished executing. *)
val poll : ('stdin, 'stdout, 'stderr) t -> Exit.status option

(** Helper funtions for constructing Cmd.t. The optional [prog]
    argument is mainly for if you don't want your path to be searched
    for the executable.

    [env] is a list of strings with the format ["NAME=value"]. [block]
    may be set to [false] for non-blocking I/O on pipes. This one
    setting is used with any pipes which are used for I/O.
    Both [env] and [block] can also be set with combinators.
*)
val cmd : ?prog:string
  -> ?env:string list
  -> ?block:bool
  -> string list
  -> (stdin, stdout, stderr) Cmd.t

val pipe_in : (stdin, 'stdout, 'stderr) Cmd.t
  -> (pipe, 'stdout, 'stderr) Cmd.t
val pipe_out : ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, pipe, 'stderr) Cmd.t
val pipe_err : ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, pipe) Cmd.t
val channel_in : in_channel
  -> (stdin, 'stdout, 'stderr) Cmd.t
  -> (channel, 'stdout, 'stderr) Cmd.t
val channel_out : out_channel
  -> ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, channel, 'stderr) Cmd.t
val channel_err : out_channel
  -> ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, channel) Cmd.t
val file_in : string
  -> (stdin, 'stdout, 'stderr) Cmd.t
  -> (file, 'stdout, 'stderr) Cmd.t
val file_out : string
  -> ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, file, 'stderr) Cmd.t
val file_err : string
  -> ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, file) Cmd.t
val append_out : string
  -> ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, append, 'stderr) Cmd.t
val append_err : string
  -> ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, append) Cmd.t
val devnull_out : ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, devnull, 'stderr) Cmd.t
val devnull_err : ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, devnull) Cmd.t
val env : string list
  -> ('stdin, 'stdout, 'stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) Cmd.t
val no_block : ('stdin, 'stdout, 'stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) Cmd.t
