open Io
module In = struct
  type _ t =
    | Stdin : stdin t
    | Channel : In_channel.t -> channel t
    | File : string -> file t
    | Pipe : pipe t

  let to_mono (type a) : a t -> Mono.t = function
    | Stdin -> Stdin
    | Channel _ -> Channel
    | File name -> File name
    | Pipe -> Pipe

  let show t = Mono.show @@ to_mono t
end

module Out = struct
  type _ t =
    | Stdout : stdout t
    | Stderr : stderr t
    | Channel : Out_channel.t -> channel t
    | File : string -> file t
    | Devnull : devnull t
    | Pipe : pipe t

  let to_mono (type a) : a t -> Mono.t = function
    | Stdout -> Stdout
    | Stderr -> Stderr
    | Channel _ -> Channel
    | File name -> File name
    | Devnull -> Devnull
    | Pipe -> Pipe

  let show t = Mono.show @@ to_mono t
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

type ('stdin, 'stdout, 'stderr) t =
  { args : string array
  ; stdin : 'stdin In.t
  ; stdout: 'stdout Out.t
  ; stderr: 'stderr Out.t
  }

module Mono = struct
  type inner =
    { args : string array
    ; stdin : Io.Mono.t
    ; stdout : Io.Mono.t
    ; stderr : Io.Mono.t 
    }
  type t = inner lazy_t

  let pp out t =
    let {args; stdin; stdout; stderr} = Lazy.force t in
    let in', out', err' = Mono.(show stdin, show stdout, show stderr) in
    let in' = if in' = "stdin" then in' else "stdin: " ^ in'
    and out' = if out' = "stdout" then out' else "stdout: " ^ out'
    and err' = if err' = "stderr" then err' else "stderr: " ^ err' in
    let open Format in
    fprintf out "@[%a@],@ %s,@ %s,@ %s"
      pp_args args in' out' err'
end

let to_mono {args; stdin; stdout; stderr} =
  lazy Mono.{ args
            ; stdin = In.to_mono stdin
            ; stdout = Out.to_mono stdout
            ; stderr = Out.to_mono stderr
            }

let pp out cmd =
  Format.fprintf out "cmd(@[%a@])" Mono.pp @@ to_mono cmd

let show cmd =
  Format.asprintf "%a" pp cmd
