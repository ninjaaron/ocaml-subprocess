module Unix = UnixLabels
open Base
open Stdio

exception Subprocess_error of string

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then
    raise (Subprocess_error "process arguments can't be empty");
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; args : string array
  ; stdin : 'stdin
  ; stdout : 'stdout
  ; stderr : 'stderr
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

module In = struct
  type _ t =
    | Fd : Unix.file_descr -> unit t
    | Channel : In_channel.t -> unit t
    | Pipe : Out_channel.t t
end

module Out = struct
  type _ t =
    | Fd : Unix.file_descr -> unit t
    | Channel : Out_channel.t -> unit t
    | Devnull : unit t
    | Pipe : In_channel.t t
end
