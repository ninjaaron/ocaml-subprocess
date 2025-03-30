module Core = Core
module Managed_in = Managed_in
include Core

let open_out cmd =
  let proc = Exec.exec (pipe_out cmd) in
  Managed_in.Pipe {proc; ic=(stdout proc)}

let open_err cmd =
  let proc = Exec.exec (pipe_err cmd) in
  Managed_in.Pipe {proc; ic=(stderr proc)}

include Functor.Make(struct
    type 'a t = 'a
    let exec cmd ~f = Exit.exn (Exec.in_context cmd ~f)
    let exec_joined cmd ~f = Exit.exn (Exec.shared_context cmd ~f)
  end)

let (let&) cmd f = exec cmd ~f

let managed_read mng =
  Exit.exn (Managed_in.all mng)

let managed_lines mng =
  Exit.exn (Managed_in.lines mng)

let _managed_either f mng =
  match f mng with
  | Either.Left ex -> Exit.exn (ex, None)
  | Right value -> Some value

let managed_line mng = _managed_either Managed_in.line mng
let managed_char mng = _managed_either Managed_in.char mng
let managed_byte mng = _managed_either Managed_in.byte mng
let managed_input mng ~buf ~pos ~len =
  match Managed_in.input mng ~buf ~pos ~len with
  | Right n -> n
  | Left ex -> Exit.exn (ex, 0)

module Results = struct
  include Functor.Make(struct
      type 'a t = ('a, Exit.t) result
      let exec cmd ~f = Exit.res (Exec.in_context cmd ~f)
      let exec_joined cmd ~f = Exit.res (Exec.shared_context cmd ~f)
    end)

  let bind cmd ~f =
    let exit, out = Exec.in_context cmd ~f in
    Result.bind out (fun x -> Exit.res (exit, x))
  let bind_joined cmd ~f =
    let exit, out = Exec.shared_context cmd ~f in
    Result.bind out (fun x -> Exit.res (exit, x))

  let string_error = Exit.string_error

  let (let*) = Result.bind
  let (let&) cmd f = bind cmd ~f

  let managed_read mng =
    Exit.res (Managed_in.all mng)

  let managed_lines mng =
    Exit.res (Managed_in.lines mng)
  include Core 
end

module Unchecked = struct
  include Functor.Make(struct
      type 'a t = Exit.t * 'a
      let exec = Exec.in_context
      let exec_joined = Exec.shared_context
    end)
  let run cmd = let exit, () = run cmd in exit
  let write cmd ~input = let exit, () = write cmd ~input in exit
  let write_lines cmd ~input = let exit, () = write_lines cmd ~input in exit
  include Core
end

module Exec = Exec
