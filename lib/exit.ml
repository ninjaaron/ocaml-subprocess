let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let status_to_string status =
  let s, i = unify_status status in
  Printf.sprintf "%s %d" s i

type t =
  { pid : int
  ; cmd : Cmd.Mono.t
  ; status : Unix.process_status
  } 

let pp out {pid; cmd; status} =
  let label, code = unify_status status in
  Format.fprintf out "(@[%s: %d,@ pid: %i,@ %a@])"
    label code pid Cmd.Mono.pp cmd

let show t =
  Format.asprintf "%a" pp t

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t

let res (t, x) =
  match t.status with
  | Unix.WEXITED 0 -> Ok x
  | _ -> Error t

let string_error res = Result.map_error show  res
let exn (t, x) = 
  match t.status with
  | Unix.WEXITED 0 -> x
  | _ -> raise (Io.Subprocess_error (show t))
