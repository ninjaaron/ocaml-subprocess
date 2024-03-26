open Printf

let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let status_to_string status =
  let s, i = unify_status status in
  sprintf "%s %d" s i

type t =
  { pid : int
  ; cmd : Cmd.Mono.t
  ; status : Unix.process_status
  } 

exception Subprocess_error of string

let () =
  Printexc.register_printer @@ function
  | Subprocess_error s ->
    Some (Printf.sprintf "Subprocess_error%s" s)
  | _ -> None

let pp out {pid; cmd; status} =
  Format.fprintf out "(@[%s,@ pid: %i,@ %a@])"
    (status_to_string status) pid Cmd.Mono.pp cmd

let show t =
  Format.asprintf "%a" pp t

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t

let string_error res = Result.map_error show  res
let exn = function
  | Ok a -> a
  | Error t -> raise (Subprocess_error (show t))
