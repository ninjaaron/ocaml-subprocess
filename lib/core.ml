module Unix = UnixLabels
module Cmd = Cmd
include Io

module In = struct
  type _ t =
    | Stdin : stdin t
    | Channel : channel t
    | File : string -> file t
    | Pipe : Out_channel.t -> pipe t

  let conv (type a) : a t -> a = function
    | Stdin -> Stdin
    | Channel -> Channel
    | File s -> File s
    | Pipe _ -> Pipe
end

module Out = struct
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : In_channel.t -> pipe t

  let conv (type a) : a t -> a = function
    | Stdout -> Stdout
    | Stderr -> Stderr
    | Channel -> Channel
    | File s -> File s
    | Devnull -> Devnull
    | Pipe _ -> Pipe
end

type ('stdin, 'stdout, 'stderr) t =
  { pid : int
  ; cmd : ('stdin, 'stdout, 'stderr) Cmd.t
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; close : ?mode:Unix.wait_flag list -> unit -> Exit.t
  }

let pp out {pid; cmd; _} =
  Format.fprintf out "process(@[pid: %d,@ %a@])"
    pid Cmd.Mono.pp @@ Cmd.to_mono cmd

let show t =
  Format.asprintf "%a" pp t

let stdin = function {stdin=In.Pipe oc; _} -> oc
let stdout = function {stdout=Out.Pipe ic; _} -> ic
let stderr = function {stderr=Out.Pipe ic; _} -> ic

let wait ?(mode = []) t = Unix.waitpid ~mode t.pid
let poll t =
  match Unix.waitpid ~mode:[ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, status -> Some status

let check t =
  Exit.check (t.close ())

let cmd args =
  if Array.length args < 1 then failwith "argument array must not be empty";
  Cmd.{ args
      ; stdin=In.Stdin
      ; stdout=Out.Stdout
      ; stderr=Out.Stderr
      }

let set_in in_t cmd = Cmd.{cmd with stdin=in_t}
let set_out out_t cmd = Cmd.{cmd with stdout=out_t}
let set_err out_t cmd = Cmd.{cmd with stderr=out_t}
let pipe_in cmd = set_in Pipe cmd
let pipe_out cmd = set_out Pipe cmd
let pipe_err cmd = set_err Pipe cmd
let channel_in ic cmd = set_in (Channel ic) cmd
let channel_out oc cmd = set_out (Channel oc) cmd
let channel_err oc cmd = set_err (Channel oc) cmd
let file_in s cmd = set_in (File s) cmd
let file_out s cmd = set_out (File s) cmd
let file_err s cmd = set_err (File s) cmd
let devnull_out cmd = set_out Devnull cmd
let devnull_err cmd = set_err Devnull cmd
