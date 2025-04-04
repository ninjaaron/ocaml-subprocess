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

exception Subprocess_error of string

module Cmd : sig
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

  type ('stdin, 'stdout, 'stderr) t =
    { args : string * string array
    ; stdin : 'stdin In.t
    ; stdout : 'stdout Out.t
    ; stderr : 'stderr Out.t
    ; env : string array
    ; block : bool
    }

  type poly = Poly : ('a, 'b, 'c) t -> poly

  val arg_to_repr : string -> string
  val pp_args : Format.formatter -> string array -> unit
  val pp : Format.formatter -> ('a, 'b, 'c) t -> unit
  [@@ocaml.toplevel_printer]
  val show : ('a, 'b, 'c) t -> string

end

module Exit : sig
  type status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  type t =
    { pid : int
    ; cmd : Cmd.poly
    ; status : status
    }

  val pp : Format.formatter -> t -> unit
  [@@ocaml.toplevel_printer]
  val show : t -> string
  val res : t * 'a -> ('a, t) result
  val exn : t * 'a -> 'a
  val string_error : ('a, t) result -> ('a, string) result
  val status_int : t -> int
end

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
  -> int * Exit.status

val poll : ('stdin, 'stdout, 'stderr) t -> Exit.status option

val cmd : ?prog:string
  -> ?env:string list
  -> ?block:bool
  -> string list
  -> (stdin, stdout, stderr) Cmd.t

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
