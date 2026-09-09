module Unix = UnixLabels

(*
   These type constructors are never used, but they are necessary
   later on for the definitions of the `stdin`, `stdout` and `stderr`
   functions to work in a way that is compatible with OCaml 5.5.0

   These are only used as phatom types, but if they are left abstract,
   5.5.0 decided that refuation cases might be undecidable because
   they could be aliased to something else in another scope, and
   therefore pattern matching on In.t for ony one variant could be
   unsound. Adding a constructor makes these proper nominal types, so
   the refutation of other cases is successful.

   This was not a problem in 5.4.1, but I suppose there was a slight
   chance of unsoundness (not in this case, but in general), and we
   don't want that.
   *)
type stdin = Stdin [@warning "-37"]
type stdout = Stdout [@warning "-37"]
type stderr = Stderr [@warning "-37"]
type channel = Channel [@warning "-37"]
type devnull = Devnull [@warning "-37"]
type file = File [@warning "-37"]
type append = Append [@warning "-37"]
type pipe = Pipe [@warning "-37"]

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
      | Devnull -> "/dev/null"
      | Pipe -> "pipe"
  end

  let arg_to_repr arg =
    let esc = String.escaped arg in
    if arg = esc && not (String.contains arg ' ')
    then arg else Printf.sprintf {|"%s"|} esc

  let pp_args out args =
    let open Format in
    pp_print_array ~pp_sep:(fun out () -> fprintf out "@ ")
      (fun out arg -> fprintf out "%s" (arg_to_repr arg))
      out
      args

  let pp_env out = function
    | [] -> ()
    | env ->
      let open Format in
      fprintf out "@[%a@]@ "
        (pp_print_list ~pp_sep:(fun out () -> fprintf out "@ ")
           (fun out (key, value)  ->
              fprintf out "%s=%s" key (arg_to_repr value)))
        env

  module T = struct
    type ('stdin, 'stdout, 'stderr) t =
      { args : string * string array
      ; stdin : 'stdin In.t
      ; stdout : 'stdout Out.t
      ; stderr : 'stderr Out.t
      ; env : (string * string) list
      ; block : bool
      }
  end
  include T

  let pp_stdin : type a. Format.formatter -> a In.t -> unit = fun out ->
    let open Format in
    function
    | In.Stdin -> ()
    | Channel _ -> fprintf out "@ < <channel>"
    | File name -> fprintf out "@ < %s" name
    | Pipe -> fprintf out "@ < <pipe>"

  let pp_stdout : type a. Format.formatter -> a Out.t -> unit = fun out ->
    let open Format in
    function
    | Out.Stdout -> ()
    | Stderr -> fprintf out "@ >&2"
    | Channel _ -> fprintf out "@ > <channel>"
    | File name -> fprintf out "@ > %s" name
    | Append name -> fprintf out "@ >> %s" name
    | Devnull -> fprintf out "@ > /dev/null"
    | Pipe -> fprintf out "@ > <pipe>"

  let pp_stderr : type a. Format.formatter -> a Out.t -> unit = fun out ->
    let open Format in
    function
    | Out.Stderr -> ()
    | Stdout -> fprintf out "@ 2>&1"
    | Channel _ -> fprintf out "@ 2> <channel>"
    | File name -> fprintf out "@ 2> %s" name
    | Append name -> fprintf out "@ 2>> %s" name
    | Devnull -> fprintf out "@ 2> /dev/null"
    | Pipe -> fprintf out "@ 2> <pipe>"

  let pp_inner
      ~show_stdout
      ~show_stdin
      out
      {args; stdin; stdout; stderr; env; _}
    = let open Format in
    pp_env out env;
    fprintf out "@[%a@]" pp_args (snd args);
    if show_stdout then
      pp_stdout out stdout;
    if show_stdin then
      pp_stdin out stdin;
    pp_stderr out stderr

  let pp out t =
    Format.fprintf out "cmd(@[`%a`"
      (pp_inner ~show_stdout:true ~show_stdin:true) t;
    if not t.block then Format.fprintf out ",@ non-blocking";
    Format.fprintf out "@])"

  let show cmd =
    Format.asprintf "%a" pp cmd
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

  type t = Exit :
    { pid : int
    ; cmd : ('a, 'b, 'c) Cmd.t
    ; status : status
    } -> t

  let status_int (Exit t) =
    match t.status with
    | WEXITED i -> i
    | WSIGNALED i -> i
    | WSTOPPED i -> i

  let pp out (Exit {pid; cmd; status}) =
    let label, code = unify_status status in
    Format.fprintf out "(@[%s: %d,@ pid: %i,@ %a@])"
      label code pid Cmd.pp cmd

  let show t =
    Format.asprintf "%a" pp t

  let res (Exit t, x) =
    match t.status with
    | WEXITED 0 -> Ok x
    | _ -> Error (Exit t)

  let string_error res = Result.map_error show  res
  let exn (Exit t, x) = 
    match t.status with
    | WEXITED 0 -> x
    | _ -> raise (Subprocess_error (show (Exit t)))
end

module In = struct
  type _ t =
    | Stdin : stdin t
    | Channel : channel t
    | File : string -> file t
    | Pipe : Out_channel.t -> pipe t
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

let stdin {stdin=In.Pipe oc; _} = oc
let stdout {stdout=Out.Pipe ic; _} = ic
let stderr {stderr=Out.Pipe ic; _} = ic

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
      ; env
      ; block
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
let env env cmd = Cmd.{cmd with env}
let no_block cmd = Cmd.{cmd with block=false}
