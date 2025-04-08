module Core = Core
include Core

include Functor.Make(struct
    type 'a t = 'a
    let exec cmd ~f = Exit.exn (Exec.in_context cmd ~f)
    let exec_joined cmd ~f = Exit.exn (Exec.shared_context cmd ~f)
  end)

let (let&) cmd f = exec cmd ~f

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
  include Core 
end

module StringResults = struct
  include Functor.Make(struct
      type 'a t = ('a, string) result
      let exec cmd ~f = Exit.string_error @@ Results.exec cmd ~f
      let exec_joined cmd ~f = Exit.string_error @@ Results.exec_joined cmd ~f
    end)

  let bind cmd ~f =
    let exit, out = Exec.in_context cmd ~f in
    Result.bind out (fun x -> Exit.(string_error @@ res (exit, x)))
  let bind_joined cmd ~f =
    let exit, out = Exec.shared_context cmd ~f in
    Result.bind out (fun x -> Exit.(string_error @@ res (exit, x)))

  let ( let* ) = Result.bind
  let ( let& ) cmd f = bind cmd ~f

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

exception Non_blocking_io_expected of string

let () = Printexc.register_printer @@ function
  | Non_blocking_io_expected s ->
    Some ("Non-blocking I/O was expected, got: " ^ s)
  | _ -> None

let check_blocking t =
  if t.cmd.block then raise @@ Non_blocking_io_expected (show t)

let read_both_proc t =
  check_blocking t;
  Functor.read_both_proc t

let fold_both_proc ?sleep t ~f ~init =
  check_blocking t;
  Functor.fold_both_proc ?sleep t ~f ~init

let fold_with_proc ?sleep ?sep t ~lines ~f ~init =
  check_blocking t;
  Functor.fold_with_proc ?sleep ?sep t ~lines ~f ~init

module Exec = Exec
