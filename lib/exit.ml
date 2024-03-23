open Printf
open StdLabels
exception Subprocess_error of string

let arg_to_repr arg =
  let esc = String.escaped arg in
  if arg = esc then arg else sprintf {|"%s"|} esc

let args_to_string args =
  String.concat ~sep:" "
  @@ List.map ~f:arg_to_repr
  @@ Array.to_list args

let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let status_to_string status =
  let s, i = unify_status status in
  sprintf "%s %d" s i

type t =
  { pid : int
  ; args : string array
  ; status : Unix.process_status
  } 

let to_string {pid; args; status} =
  Printf.sprintf "pid: %d, status: %s, %s"
    pid (status_to_string status) (args_to_string args)

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t

let string_error res = Result.map_error to_string res
let exn = function
  | Ok a -> a
  | Error t -> raise (Subprocess_error (to_string t))
