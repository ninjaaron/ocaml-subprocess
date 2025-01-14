module Core = Core
module Cmd = Cmd
include Core

module type Exec_t = sig
  type 'a t
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) Core.t -> 'a) ->
    'a t
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) Core.t -> 'a) ->
    'a t
end

module type S = sig
  type 'a t
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) Core.t -> 'a) ->
    'a t
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) Core.t -> 'a) ->
    'a t
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> unit t
  val read : ('stdin, stdout, 'stderr) Cmd.t -> string t
  val lines : ('stdin, stdout, 'stderr) Cmd.t -> string list t
  val read_err : ('stdin, 'stdout, stderr) Cmd.t -> string t
  val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> string list t
  val read_joined : ('stdin, stdout, stderr) Cmd.t -> string t
  val lines_joined : ('stdin, stdout, stderr) Cmd.t -> string list t
  val fold : ('stdin, stdout, 'stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
  val fold_err : ('stdin, 'stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
  val fold_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
end

let run_f _ = ()
let read_f stream t = In_channel.input_all (stream t)
let lines_f stream t = In_channel.input_lines (stream t)
let fold_f stream f init t = In_channel.fold_lines f init (stream t)

module Make(M: Exec_t) : S with type 'a t := 'a M.t = struct
  include M
  let run cmd = exec cmd ~f:run_f
  let read cmd = exec (pipe_out cmd) ~f:(read_f stdout)
  let lines cmd = exec (pipe_out cmd) ~f:(lines_f stdout)
  let read_err cmd = exec (pipe_err cmd) ~f:(read_f stderr)
  let lines_err cmd = exec (pipe_err cmd) ~f:(lines_f stderr)
  let read_joined cmd = exec_joined cmd ~f:(read_f stdout)
  let lines_joined cmd = exec_joined cmd ~f:(lines_f stdout)
  let fold cmd ~f ~init =
    exec (pipe_out cmd) ~f:(fold_f stdout f init)
  let fold_err cmd ~f ~init =
    exec (pipe_err cmd) ~f:(fold_f stderr f init)
  let fold_joined cmd ~f ~init =
    exec_joined cmd ~f:(fold_f stdout f init)
end

include Make(struct
    type 'a t = 'a
    let exec cmd ~f = Exit.exn (Exec.in_context cmd ~f)
    let exec_joined cmd ~f = Exit.exn (Exec.shared_context cmd ~f)
  end)

let (let&) cmd f = exec cmd ~f

module Results = struct
  include Core 
  include Make(struct
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

end

module Unchecked = struct
  include Core
  include Make(struct
      type 'a t = Exit.t * 'a
      let exec = Exec.in_context
      let exec_joined = Exec.shared_context
    end)
  let run cmd = let exit, () = run cmd in exit
end


module Exec = Exec
