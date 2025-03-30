module Unix = UnixLabels

type stdin = Stdin
type stdout = Stdout
type stderr = Stderr
type channel = Channel
type devnull = Devnull
type file = File of string
type append = Append of string
type pipe = Pipe

exception Subprocess_error of string

let () =
  Printexc.register_printer @@ function
  | Subprocess_error s ->
    Some (Printf.sprintf "Subprocess_error%s" s)
  | _ -> None

module Cmd = struct
  module In = struct
    type _ t =
      | Stdin : stdin t
      | Channel : In_channel.t -> channel t
      | File : string -> file t
      | Pipe : pipe t

    let show : type a. a t -> string = function
      | Stdin -> "stdin"
      | Channel _ -> "channel"
      | File s -> Printf.sprintf {|file "%s"|} (String.escaped s)
      | Pipe -> "pipe"
  end

  module Out = struct
    type _ t =
      | Stdout : stdout t
      | Stderr : stderr t
      | Channel : Out_channel.t -> channel t
      | File : string -> file t
      | Append : string -> append t
      | Devnull : devnull t
      | Pipe : pipe t

    let show (type a) : a t -> string = function
      | Stdout -> "stdout"
      | Stderr -> "stderr"
      | Channel _ -> "channel"
      | File s -> Printf.sprintf {|file "%s"|} (String.escaped s)
      | Append s -> Printf.sprintf {|append "%s"|} (String.escaped s)
      | Devnull -> "devnull"
      | Pipe -> "pipe"
  end

  let arg_to_repr arg =
    let esc = String.escaped arg in
    if arg = esc && not (String.contains arg ' ')
    then arg else Printf.sprintf {|"%s"|} esc

  let pp_args out args =
    let open Format in
    fprintf out "`%a`" 
      (pp_print_array ~pp_sep:(fun out () -> fprintf out "@ ")
         (fun out arg -> fprintf out "%s" (arg_to_repr arg)))
      args

  let pp_env out = function
    | [||] -> ()
    | env ->
      let open Format in
      fprintf out "env:[@[%a@]]"
        (pp_print_array ~pp_sep:(fun out () -> fprintf out ",@ ")
           (fun out arg -> fprintf out "%s" (arg_to_repr arg)))
        env


  module T = struct
    type ('stdin, 'stdout, 'stderr) t =
      { args : string * string array
      ; stdin : 'stdin In.t
      ; stdout : 'stdout Out.t
      ; stderr : 'stderr Out.t
      ; env : string array
      ; block : bool
      }
  end
  include T

  let pp_io out streams =
    let streams' = ListLabels.filter_map streams
        ~f:(fun (default, s) ->
            if s = default then None else Some (default ^ ": " ^ s)) in
    if List.is_empty streams' then ()
    else Format.fprintf out ",@ ";
    Format.(pp_print_list
              ~pp_sep:(fun out () -> Format.fprintf out ",@ ")
              Format.pp_print_string
              out
              streams')

  let pp out t =
    let {args; stdin; stdout; stderr; env; block} = t in
    Format.fprintf out "@[%a@]%a"
      pp_args (snd args)
      pp_io [ "stdin", In.show stdin
            ; "stdout", Out.show stdout
            ; "stderr", Out.show stderr];
    (match env with
     | [||] -> ()
     | _ ->
       Format.fprintf out "@ @[%a@]" pp_env env);
    if not block then Format.fprintf out "@ non-blocking"

  let show cmd =
    Format.asprintf "%a" pp cmd

  type poly = Poly : ('a, 'b, 'c) T.t -> poly
end

module Exit = struct
  type status = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int
                  
  let unify_status = function
    | WEXITED i -> "exited", i
    | WSIGNALED i -> "signaled", i
    | WSTOPPED i -> "stopped", i

  type t =
    { pid : int
    ; cmd : Cmd.poly
    ; status : status
    } 

  let status_int t =
    match t.status with
    | WEXITED i -> i
    | WSIGNALED i -> i
    | WSTOPPED i -> i

  let pp out {pid; cmd=Cmd.Poly cmd; status} =
    let label, code = unify_status status in
    Format.fprintf out "(@[%s: %d,@ pid: %i,@ %a@])"
      label code pid Cmd.pp cmd

  let show t =
    Format.asprintf "%a" pp t

  let res (t, x) =
    match t.status with
    | WEXITED 0 -> Ok x
    | _ -> Error t

  let string_error res = Result.map_error show  res
  let exn (t, x) = 
    match t.status with
    | WEXITED 0 -> x
    | _ -> raise (Subprocess_error (show t))
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
    | Append : string -> append t
    | Devnull : devnull t
    | Pipe : In_channel.t -> pipe t

  let conv (type a) : a t -> a = function
    | Stdout -> Stdout
    | Stderr -> Stderr
    | Channel -> Channel
    | File s -> File s
    | Append s -> Append s
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
    pid Cmd.pp cmd

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

let cmd ?prog ?(env=[]) ?(block=true) args =
  if List.is_empty args then failwith "argument array must not be empty";
  let args = Array.of_list args in
  let prog = match prog with
    | None -> args.(0)
    | Some s -> s in
  Cmd.{ args = prog, args
      ; stdin = In.Stdin
      ; stdout = Out.Stdout
      ; stderr = Out.Stderr
      ; env = Array.of_list env
      ; block = block
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
let append_out s cmd = set_out (Append s) cmd
let append_err s cmd = set_err (Append s) cmd
let devnull_out cmd = set_out Devnull cmd
let devnull_err cmd = set_err Devnull cmd
let env env_list cmd = Cmd.{cmd with env=Array.of_list env_list}
let no_block cmd = Cmd.{cmd with block=false}
