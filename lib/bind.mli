open Core

val exit_t : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val exit_t_both : ('stdin, stdout, stderr) Cmd.t
  -> f:(('stdin, pipe, stdout) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val exn : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> 'a) -> 'a

val exn_both : ('stdin, stdout, stderr) Cmd.t
  -> f:(('stdin, pipe, stdout) t -> 'a) -> 'a
