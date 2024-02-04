open Base

let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let status_to_string status =
  let s, i = unify_status status in
  Printf.sprintf "%s %d" s i

let re_status = function
  | "exited", i -> Unix.WEXITED i
  | "signaled", i -> Unix.WSIGNALED i
  | "stopped", i -> Unix.WSTOPPED i
  | _ -> failwith "should't get here"

type t =
  { pid : int
  ; args : string array
  ; status : Unix.process_status
  } 

let sexp_of_t {pid; args; status} =
  [%sexp
    { pid = (pid : int)
    ; args = (args : string array)
    ; status = (unify_status status : string * int)
    } ]

let t_of_sexp sexp =
  let open Sexp in
  match sexp with
  | List [ List [Atom "pid"; pid]
         ; List [Atom "args"; args]
         ; List [Atom "status"; List [s; i]]
         ] ->
    { pid = Int.t_of_sexp pid
    ; args = array_of_sexp string_of_sexp args
    ; status = re_status (string_of_sexp s, int_of_sexp i)
    }
  | s -> failwith ("bad sexp, apparently: " ^ Sexp.to_string s)

let to_string {pid; args; status} =
  Printf.sprintf "pid: %d, status: %s\n%s"
    pid (status_to_string status) (Args.to_string args)

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t
