open Core

(** Start a process and return the handle (an instance of
    {!Subprocess.t}). This process will need to be closed manually, but the
    recored carres around a [close] "method" (actually a closure)
    which, when applied to [()], will close any open file descriptors
    attached to the process (except for those passed in as a channel).

    Applying the [close] property of a {!Subprocess.t} instance returns an
    instance of {!Exit.t}.
*)
val exec : ('stdin, 'stdout, 'stderr) Cmd.t -> ('stdin, 'stdout, 'stderr) t


(** Open a command in context of a function, and close automaticall
    afterwards, automatically closing everything at the end *)
val in_context : ('stdin, 'stdout, 'stderr) Cmd.t
  -> f:(('stdin, 'stdout, 'stderr) t -> 'a)
  -> Exit.t * 'a

(** Same as {!exec}, but recirects stdout and stderr to the same pipe.
    Similar to using [2>&1 |] in the shell. *)
val shared_pipe : ('stdin, stdout, stderr) Cmd.t -> ('stdin, pipe, stdout) t

(** Same as {!in_context} but with the redirection mentioned in
    {!shared_pipe}. *)
val shared_context : ('stdin, stdout, stderr) Cmd.t
  -> f:(('stdin, pipe, stdout) t -> 'a)
  -> Exit.t * 'a
