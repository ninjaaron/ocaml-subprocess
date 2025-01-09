open Core

val exec : ('stdin, 'stdout, 'stderr) Cmd.t -> ('stdin, 'stdout, 'stderr) t
val in_context : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> 'a)
  -> Exit.t * 'a
val shared_pipe : ('stdin, stdout, stderr) Cmd.t -> ('stdin, pipe, pipe) t
val shared_context : ('stdin, stdout, stderr) Cmd.t
  -> f:(('stdin, pipe, pipe) t -> 'a)
  -> Exit.t * 'a
