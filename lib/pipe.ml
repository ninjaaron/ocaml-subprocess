open Core

module T = struct
  type (_, _, _) t =
    | Single : ('i, 'o, 'e) Cmd.t -> ('i, 'o, 'e) t
    | Pipe : ('i, stdout, _) Cmd.t * (stdin, 'o, 'e) t -> ('i, 'o, 'e) t
end

include T

let pipe cmd t = Pipe (cmd, t)
let (@|) = pipe

let pipe_in = function
  | Single cmd -> Single (Core.pipe_in cmd)
  | Pipe (cmd, t2) -> Pipe (Core.pipe_in cmd, t2)

let rec pipe_out : type i e. (i, stdout, e) t -> (i, pipe, e) t =
  function
  | Single cmd -> Single (Core.pipe_out cmd)
  | Pipe (cmd, t2) -> Pipe (cmd, pipe_out t2)

let rec pipe_err : type i o. (i, o, stderr) t -> (i, o, pipe) t =
  function
  | Single cmd -> Single (Core.pipe_err cmd)
  | Pipe (cmd, t2) -> Pipe (cmd, pipe_err t2)

module Exec = struct
  type (_, _, _) t =
  | Single : ('i, 'o, 'e) Core.t -> ('i, 'o, 'e) t
  | Pipe : ('i, pipe, _) Core.t * (channel, 'o, 'e) t -> ('i, 'o, 'e) t

  let rec close
    : type i o e. ?mode:Unix.wait_flag list -> (i, o, e) t-> Exit.t =
    fun ?mode -> function
    | Single proc -> proc.close ?mode ()
    | Pipe (proc, t2) ->
      let e = proc.close ?mode () in
      match Exit.status_int e with
      | 0 -> close ?mode t2
      | _ -> ignore (close ?mode t2); e

  let stdin = function
    | Single proc -> Core.stdin proc
    | Pipe (t, _) -> stdin t

  let rec stdout : type i e. (i, pipe, e) t -> in_channel =
    function
    | Single proc -> Core.stdout proc
    | Pipe (_, t) -> stdout t

  let rec stderr : type i o. (i, o, pipe) t -> in_channel =
    function
    | Single proc -> Core.stderr proc
    | Pipe (_, t) -> stderr t

  let _ = Exec.exec

  let exec tt =
    let rec loop : type i o e. in_channel -> (stdin, o, e) T.t -> (channel, o, e) t =
      fun ic -> function
        | Single cmd -> Single (Exec.exec @@ Core.channel_in ic cmd)
        | Pipe (cmd, t) ->
          let proc = Exec.exec @@ Core.channel_in ic @@ Core.pipe_out cmd in
          Pipe(proc, loop (Core.stdout proc) t) in
    match tt with
    | T.Single cmd -> Single (Exec.exec cmd)
    | Pipe (cmd, p2) ->
      let proc = Exec.exec (Core.pipe_out cmd) in
      Pipe (proc, loop (Core.stdout proc) p2)

  let in_context tt ~f =
    let t = exec tt in
    match f t with
    | output -> close t, output
    | exception e ->
      let _ = close t in
      raise e
end
