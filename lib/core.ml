module Unix = UnixLabels
open Base
open Stdio

exception Subprocess_error of string

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; args : string array
  ; stdin : 'stdin
  ; stdout : 'stdout
  ; stderr : 'stderr
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

type stdin = unit
type stdout = unit
type stderr = unit
type channel = unit
type devnull = unit

let stdin = ()
let stdout = ()
let stderr = ()
let channel = ()
let devnull = ()

module In = struct
  type pipe = Out_channel.t
  type _ t =
    | Stdin : stdin t
    | Channel : In_channel.t -> channel t
    | Pipe : pipe t
end

module Out = struct
  type pipe = In_channel.t
  type _ t =
    | Stdout : stdout t
    | Stderr : stdout t
    | Channel : Out_channel.t -> channel t
    | Devnull : devnull t
    | Pipe : pipe t
end

module Cmd = struct
  type ('stdin, 'stdout, 'stderr) t =
    { args : string array
    ; stdin : 'stdin In.t
    ; stdout: 'stdout Out.t
    ; stderr: 'stderr Out.t
    }
end
