exception Subprocess_error of string

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; args : string array
  ; stdin : 'stdin
  ; stdout : 'stdout
  ; stderr : 'stderr
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

type stdin = Stdin
type stdout = Stdout
type stderr = Stderr
type channel = Channel
type devnull = Devnull
type file = File of string

module In : sig
  type _ t =
    | Stdin : stdin t
    | Channel : in_channel -> channel t
    | File : string -> file t
    | Pipe : out_channel t
end

module Out : sig
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : out_channel -> channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : in_channel t
end

module Cmd : sig
  type ('stdin, 'stdout, 'stderr) t =
    { args : string array
    ; stdin : 'stdin In.t
    ; stdout: 'stdout Out.t
    ; stderr: 'stderr Out.t
    }
end

(* val set_in : 'i2 In.t -> ('i1, 'o, 'e) Cmd.t -> ('i2, 'o, 'e) Cmd.t *)
(* val set_out : 'o2 Out.t -> ('i, 'o1, 'e) Cmd.t -> ('i, 'o2, 'e) Cmd.t *)
(* val set_err : 'e2 Out.t -> ('i, 'o, 'e1) Cmd.t -> ('i, 'o, 'e2) Cmd.t *)
val pipe_in : (stdin, 'o, 'e) Cmd.t -> (out_channel, 'o, 'e) Cmd.t
val pipe_out : ('i, stdout, 'e) Cmd.t -> ('i, in_channel, 'e) Cmd.t
val pipe_err : ('i, 'o, stderr) Cmd.t -> ('i, 'o, in_channel) Cmd.t
val channel_in : in_channel -> (stdin, 'o, 'e) Cmd.t -> (channel, 'o, 'e) Cmd.t
val channel_out : out_channel -> ('i, stdout, 'e) Cmd.t -> ('i, channel, 'e) Cmd.t
val channel_err : out_channel -> ('i, 'o, stderr) Cmd.t -> ('i, 'o, channel) Cmd.t
val file_in : string -> (stdin, 'o, 'e) Cmd.t -> (file, 'o, 'e) Cmd.t
val file_out : string -> ('i, stdout, 'e) Cmd.t -> ('i, file, 'e) Cmd.t
val file_err : string -> ('i, 'o, stderr) Cmd.t -> ('i, 'o, file) Cmd.t
val devnull_out : ('i, stdout, 'e) Cmd.t -> ('i, devnull, 'e) Cmd.t
val devnull_err : ('i, 'o, stderr) Cmd.t -> ('i, 'o, devnull) Cmd.t
