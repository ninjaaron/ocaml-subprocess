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
type file = File of string

let stdin = ()
let stdout = ()
let stderr = ()
let channel = ()
let devnull = ()

module In = struct
  type _ t =
    | Stdin : stdin t
    | Channel : In_channel.t -> channel t
    | File : string -> file t
    | Pipe : Out_channel.t t
end

module Out = struct
  type _ t =
    | Stdout : stdout t
    | Stderr : stdout t
    | Channel : Out_channel.t -> channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : In_channel.t t
end

module Cmd = struct
  type ('stdin, 'stdout, 'stderr) t =
    { args : string array
    ; stdin : 'stdin In.t
    ; stdout: 'stdout Out.t
    ; stderr: 'stderr Out.t
    }
end
