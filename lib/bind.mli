open Core

val exit_t : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val string_error : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> ('a, string) result)
  -> ('a, string) result

val exn : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> 'a) -> 'a
