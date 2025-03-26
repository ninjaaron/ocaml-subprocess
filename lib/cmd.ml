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

let pp_env out = function
  | [||] -> ()
  | env ->
    let open Format in
    fprintf out "env:[@[%a@]]"
    (pp_print_array ~pp_sep:(fun out () -> fprintf out ",@ ")
       (fun out arg -> fprintf out "%s" (arg_to_repr arg)))
    env
      

type ('stdin, 'stdout, 'stderr) t =
  { args : string array
  ; stdin : 'stdin In.t
  ; stdout : 'stdout Out.t
  ; stderr : 'stderr Out.t
  ; env : string array
  ; block : bool
  }

module Mono = struct
  type inner =
    { args : string array
    ; stdin : Io.Mono.t
    ; stdout : Io.Mono.t
    ; stderr : Io.Mono.t 
    ; env : string array
    ; block : bool
    }
  type t = inner lazy_t

  let pp_io out streams =
    let streams' = ListLabels.filter_map streams
      ~f:(fun (default, stream) ->
          match Mono.show stream with
          | s when s = default -> None
          | s -> Some (default ^ ": " ^ s)) in
    if List.is_empty streams' then ()
    else Format.fprintf out ",@ ";
    Format.(pp_print_list
              ~pp_sep:(fun out () -> Format.fprintf out ",@ ")
              Format.pp_print_string
              out
              streams')

  let pp out t =
    let {args; stdin; stdout; stderr; env; block} = Lazy.force t in
    Format.fprintf out "@[%a@]%a"
      pp_args args
      pp_io ["stdin", stdin; "stdout", stdout; "stderr", stderr];
    (match env with
     | [||] -> ()
     | _ ->
      Format.fprintf out "@ @[%a@]" pp_env env);
    if not block then Format.fprintf out "@ non-blocking"
      
end

let to_mono {args; stdin; stdout; stderr; env; block} =
  lazy Mono.{ args
            ; stdin = In.to_mono stdin
            ; stdout = Out.to_mono stdout
            ; stderr = Out.to_mono stderr
            ; env
            ; block
            }

let pp out cmd =
  Format.fprintf out "cmd(@[%a@])" Mono.pp @@ to_mono cmd

let show cmd =
  Format.asprintf "%a" pp cmd
