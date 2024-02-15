type stdin type stdout type stderr
type channel type devnull type file type pipe

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
    | Devnull : devnull t
    | Pipe : In_channel.t -> pipe t
end

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; args : string array
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

val stdin : (pipe, 'stdout, 'stderr) t -> out_channel
val stdout : ('stdin, pipe, 'stderr) t -> in_channel
val stderr : ('stdin, 'stdout, pipe) t -> in_channel

val wait : ?mode:Unix.wait_flag list
  -> ('stdin, 'stdout, 'stderr) t
  -> int * Unix.process_status

val poll : ('stdin, 'stdout, 'stderr) t -> Unix.process_status option

val check : ('stdin, 'stdout, 'stderr) t -> (Exit.t, Exit.t) result

module Cmd : sig
  module In : sig
    type _ t =
      | Stdin : stdin t
      | Channel : in_channel -> channel t
      | File : string -> file t
      | Pipe : pipe t
  end

  module Out : sig
    type _ t =
      | Stdout : stdout t
      | Stderr : stderr t
      | Channel : out_channel -> channel t
      | File : string -> file t
      | Devnull : devnull t
      | Pipe : pipe t
  end

  type ('stdin, 'stdout, 'stderr) t =
    { args : string array
    ; stdin : 'stdin In.t
    ; stdout: 'stdout Out.t
    ; stderr: 'stderr Out.t
    }
end

val cmd : string array -> (stdin, stdout, stderr) Cmd.t

val set_in : 'stdin2 Cmd.In.t
  -> ('stdin1, 'stdout, 'stderr) Cmd.t
  -> ('stdin2, 'stdout, 'stderr) Cmd.t
val set_out : 'stdout2 Cmd.Out.t
  -> ('stdin, 'stdout1, 'stderr) Cmd.t
  -> ('stdin, 'stdout2, 'stderr) Cmd.t
val set_err : 'stderr2 Cmd.Out.t
  -> ('stdin, 'stdout, 'stderr1) Cmd.t
  -> ('stdin, 'stdout, 'stderr2) Cmd.t
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
