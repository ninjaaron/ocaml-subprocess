module Types = struct
  type stdin = Stdin
  type stdout = Stdout
  type stderr = Stderr
  type channel = Channel
  type devnull = Devnull
  type file = File of string
  type pipe = Pipe

  exception Subprocess_error of string
end
include Types

let () =
  Printexc.register_printer @@ function
  | Subprocess_error s ->
    Some (Printf.sprintf "Subprocess_error%s" s)
  | _ -> None

module Mono = struct
  type t =
    | Stdin
    | Stdout
    | Stderr
    | Channel
    | Devnull
    | File of string
    | Pipe

  let show = function
    | Stdin -> "stdin"
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Channel -> "channel"
    | Devnull -> "devnull"
    | File s -> Printf.sprintf {|file "%s"|} (String.escaped s)
    | Pipe -> "pipe"
end
