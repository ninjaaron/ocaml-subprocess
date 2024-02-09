open Core
type ('stdin, 'stdout, 'stderr) t =
  { pid: int
  ; args: string array
  ; status: Unix.process_status
  ; stdin: 'stdin
  ; stdout: 'stdout
  ; stderr: 'stderr
  }
val get_exit : ('stdin, 'stdout, 'stderr) t -> Exit.t

module Read : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> ('stdin, string, 'stderr) t
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> ('stdin, 'stdout, string) t
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> ('stdin, string, string) t
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> (('stdin, string, string) t, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> ('stdin, string, string) t
end

module Lines : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> ('stdin, string list, 'stderr) t
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string list, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string list
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> ('stdin, 'stdout, string list) t
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string list, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string list
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> ('stdin, string list, string list) t
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> (('stdin, string list, string list) t, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> ('stdin, string list, string list) t
end

val unchecked : ('stdin, 'stdout, 'stderr) Cmd.t
  -> ('stdin, 'stdout, 'stderr) t
val res : ('stdin, 'stdout, 'stderr) Cmd.t -> (unit, Exit.t) result
val exn : ('stdin, 'stdout, 'stderr) Cmd.t -> unit
