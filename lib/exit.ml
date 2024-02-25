exception Subprocess_error of string

let args_to_string args =
  let s = Sexp.Of.(array ~f:string args |> Sexp.show) in
  StringLabels.sub ~pos:1 ~len:(String.length s - 2) s

let unify_status = function
  | Unix.WEXITED i -> "exited", i
  | Unix.WSIGNALED i -> "signaled", i
  | Unix.WSTOPPED i -> "stopped", i

let re_status = function
  | "exited", i -> Unix.WEXITED i
  | "signaled", i -> Unix.WSIGNALED i
  | "stopped", i -> Unix.WSTOPPED i
  | _ -> failwith "should't get here"

let status_to_string status =
  let s, i = unify_status status in
  Printf.sprintf "%s %d" s i

let sexp_of_status status =
  let s, i = unify_status status in
  Sexp.(List [Atom s; Of.int i])

let status_of_sexp = function
  | Sexp.(List [Atom s; i]) ->
    re_status (s, Sexp.To.int i)
  | _ -> failwith "not a status sexp"

type t =
  { pid : int
  ; args : string array
  ; status : Unix.process_status
  } 

let to_string {pid; args; status} =
  Printf.sprintf "pid: %d, status: %s\n%s"
    pid (status_to_string status) (args_to_string args)

let sexp_of_t {pid; args; status} =
  let open Sexp.Of in
  Sexp.List
    [ field ("pid", pid) int
    ; field ("args", args) (array ~f:string)
    ; field ("status", status) sexp_of_status
    ]

let t_of_sexp sexp =
  let open Sexp.To in
  match sexp with
  | Sexp.List [ pid' ; args' ; status'] ->
    { pid = field "pid" pid' int
    ; args = field "args" args' (array ~f:string)
    ; status = field "status" status' status_of_sexp
    }
  | _ -> failwith "not a valid Exit sexp"

let check t =
  match t.status with
  | Unix.WEXITED 0 -> Ok t
  | _ -> Error t

let string_error res = Result.map_error to_string res
let exn = function
  | Ok a -> a
  | Error t -> raise (Subprocess_error (to_string t))
