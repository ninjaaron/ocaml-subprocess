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

include module type of Io.Types

module In : sig
  type _ t =
    | Stdin : stdin t
    | Channel : channel t
    | File : string -> file t
    | Pipe : Out_channel.t -> pipe t

  val conv : 'a t -> 'a
end

module Out : sig
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : In_channel.t -> pipe t
  val conv : 'a t -> 'a
end


(** The type {!type:t} represents a process which may still be
    running. Some of the higher-level functions don't expose it
    directly, but almost everything else is implemented in terms of
    this type *)
type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; cmd : ('stdin, 'stdout, 'stderr) Cmd.t
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

val pp : Format.formatter -> ('stdin, 'stdout, 'stderr) t -> unit
[@@ocaml.toplevel_printer]

val show : ('stdin, 'stdout, 'stderr) t -> string

val stdin : (pipe, 'stdout, 'stderr) t -> out_channel
val stdout : ('stdin, pipe, 'stderr) t -> in_channel
val stderr : ('stdin, 'stdout, pipe) t -> in_channel

val wait : ?mode:Unix.wait_flag list
  -> ('stdin, 'stdout, 'stderr) t
  -> int * Unix.process_status

val poll : ('stdin, 'stdout, 'stderr) t -> Unix.process_status option

val check : ('stdin, 'stdout, 'stderr) t -> (Exit.t, Exit.t) result

val cmd : string list -> (stdin, stdout, stderr) Cmd.t

val set_in : 'stdin Cmd.In.t
  -> (stdin, 'stdout, 'stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) Cmd.t
val set_out : 'stdout Cmd.Out.t
  -> ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) Cmd.t
val set_err : 'stderr Cmd.Out.t
  -> ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) Cmd.t
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
val devnull_out : ('stdin, stdout, 'stderr) Cmd.t
  -> ('stdin, devnull, 'stderr) Cmd.t
val devnull_err : ('stdin, 'stdout, stderr) Cmd.t
  -> ('stdin, 'stdout, devnull) Cmd.t
