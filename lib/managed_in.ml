type t =
    Pipe : { proc: ('a, 'b, 'c) Core.t ; ic: In_channel.t} -> t

let close (Pipe {proc; _}) =
  proc.close ()

let _input f (Pipe {proc; ic}) =
  match f ic with
  | Some x -> Either.Right x
  | None -> 
    Left (proc.close ())

let char t = _input In_channel.input_char t
let line t = _input In_channel.input_line t
let byte t = _input In_channel.input_byte t

let _input_all f (Pipe {proc; ic}) =
  let out = f ic in
  proc.close (), out

let all t = _input_all In_channel.input_all t
let lines t = _input_all In_channel.input_lines t

let input (Pipe {proc; ic}) ~buf ~pos ~len =
  match In_channel.input ic buf pos len with
  | 0 -> Either.Left (proc.close ())
  | n -> Right n
