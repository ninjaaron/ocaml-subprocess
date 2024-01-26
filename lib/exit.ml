let status_to_string = function
  | Unix.WEXITED i -> "exited " ^ Int.to_string i
  | Unix.WSIGNALED i -> "signaled " ^ Int.to_string i
  | Unix.WSTOPPED i -> "stopped " ^ Int.to_string i

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
