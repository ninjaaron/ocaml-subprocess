open Io

module Read : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> Exit.t * string
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> Exit.t * string
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> Exit.t * string
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> (string, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> string
end

module Lines : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> Exit.t * string list
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string list, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string list
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> Exit.t * string list
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string list, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string list
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> Exit.t * string list
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> (string list, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> string list
end

val unchecked : ('stdin, 'stdout, 'stderr) Cmd.t
  -> Exit.t
val res : ('stdin, 'stdout, 'stderr) Cmd.t -> (unit, Exit.t) result
val exn : ('stdin, 'stdout, 'stderr) Cmd.t -> unit
