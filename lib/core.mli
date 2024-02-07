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
type file = File of string

val stdin : stdin
val stdout : stdout
val stderr : stderr
val channel : channel
val devnull : devnull

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
