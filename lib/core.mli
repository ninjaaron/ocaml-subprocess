exception Subprocess_error of string

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; args : string array
  ; stdin : 'stdin
  ; stdout : 'stdout
  ; stderr : 'stderr
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

type stdin
type stdout
type stderr
type channel
type devnull

val stdin : stdin
val stdout : stdout
val stderr : stderr
val channel : channel
val devnull : devnull

module In : sig
  type pipe = Out_channel.t
  type _ t =
    | Stdin : stdin t
    | Channel : In_channel.t -> channel t
    | Pipe : pipe t
end

module Out : sig
  type pipe = In_channel.t
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : Out_channel.t -> channel t
    | Devnull : devnull t
    | Pipe : pipe t
end

module Cmd : sig
  type ('stdin, 'stdout, 'stderr) t =
    { args : string array
    ; stdin : 'stdin In.t
    ; stdout: 'stdout Out.t
    ; stderr: 'stderr Out.t
    }
end
