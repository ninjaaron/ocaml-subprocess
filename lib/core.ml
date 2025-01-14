module Unix = UnixLabels
include Io

module Exit = struct
  let unify_status = function
    | Unix.WEXITED i -> "exited", i
    | Unix.WSIGNALED i -> "signaled", i
    | Unix.WSTOPPED i -> "stopped", i

  type t =
    { pid : int
    ; cmd : Cmd.Mono.t
    ; status : Unix.process_status
    } 

  let status_int t =
    match t.status with
    | Unix.WEXITED i -> i
    | Unix.WSIGNALED i -> i
    | Unix.WSTOPPED i -> i

  let pp out {pid; cmd; status} =
    let label, code = unify_status status in
    Format.fprintf out "(@[%s: %d,@ pid: %i,@ %a@])"
      label code pid Cmd.Mono.pp cmd

  let show t =
    Format.asprintf "%a" pp t

  let res (t, x) =
    match t.status with
    | Unix.WEXITED 0 -> Ok x
    | _ -> Error t

  let string_error res = Result.map_error show  res
  let exn (t, x) = 
    match t.status with
    | Unix.WEXITED 0 -> x
    | _ -> raise (Io.Subprocess_error (show t))
end

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

let cmd args =
  if List.is_empty args then failwith "argument array must not be empty";
  Cmd.{ args=Array.of_list args
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
