exception Subprocess_error of string

let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let status_to_string status =
  let s, i = unify_status status in
  Printf.sprintf "%s %d" s i

(* let re_status = function *)
(*   | "exited", i -> Unix.WEXITED i *)
(*   | "signaled", i -> Unix.WSIGNALED i *)
(*   | "stopped", i -> Unix.WSTOPPED i *)
(*   | _ -> failwith "should't get here" *)

type t =
  { pid : int
  ; args : string array
  ; status : Unix.process_status
  } 

let to_string {pid; args; status} =
  Printf.sprintf "pid: %d, status: %s\n%s"
    pid (status_to_string status) (Args.to_string args)

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t

let string_error res = Result.map_error to_string res
let exn = function
  | Ok a -> a
  | Error t -> raise (Subprocess_error (to_string t))
