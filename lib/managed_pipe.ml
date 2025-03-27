module Managed = struct
  type t = Pipe : { proc: ('a, 'b, 'c) Core.t ; ic: In_channel.t} -> t
end

open Managed

let close (Pipe {proc; _}) =
  proc.close ()

let _input f (Pipe {proc; ic}) =
  match f ic with
  | Some x -> Some x
  | None -> 
    Core.Exit.exn (proc.close (), None)

let input_char t = _input In_channel.input_char t
let input_line t = _input In_channel.input_line t
let input_byte t = _input In_channel.input_byte t

let _input_all f (Pipe {proc; ic}) =
  let out = f ic in
  Core.Exit.exn (proc.close (), out)

let input_all t = _input_all In_channel.input_all t
let input_lines t = _input_all In_channel.input_lines t

let input (Pipe {proc; ic}) ~buf ~pos ~len =
  match In_channel.input ic buf pos len with
  | 0 -> Core.Exit.exn (proc.close (), 0)
  | n -> n
