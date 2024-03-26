open Io
type ('stdout, 'stderr) t =
  { pid: int
  ; cmd: Cmd.Mono.t
  ; status: Unix.process_status
  ; stdout: 'stdout
  ; stderr: 'stderr
  }
val get_exit : ('stdout, 'stderr) t -> Exit.t

val pp : Format.formatter -> ('b, 'c) t -> unit
[@@ocaml.toplevel_printer]
val show : ('b, 'c) t -> string

module Read : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> (string, 'stderr) t
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> ('stdout, string) t
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> (string, string) t
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> ((string, string) t, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> (string, string) t
end

module Lines : sig
  val unchecked : ('stdin, stdout, 'stderr) Cmd.t
    -> (string list, 'stderr) t
  val res : ('stdin, stdout, 'stderr) Cmd.t
    -> (string list, Exit.t) result
  val exn : ('stdin, stdout, 'stderr) Cmd.t -> string list
  val err_unchecked : ('stdin, 'stdout, stderr) Cmd.t
    -> ('stdout, string list) t
  val err_res : ('stdin, 'stdout, stderr) Cmd.t
    -> (string list, Exit.t) result
  val err_exn : ('stdin, 'stdout, stderr) Cmd.t -> string list
  val both_unchecked : ('stdin, stdout, stderr) Cmd.t
    -> (string list, string list) t
  val both_res : ('stdin, stdout, stderr) Cmd.t
    -> ((string list, string list) t, Exit.t) result
  val both_exn : ('stdin, stdout, stderr) Cmd.t
    -> (string list, string list) t
end

val unchecked : ('stdin, 'stdout, 'stderr) Cmd.t
  -> ('stdout, 'stderr) t
val res : ('stdin, 'stdout, 'stderr) Cmd.t -> (unit, Exit.t) result
val exn : ('stdin, 'stdout, 'stderr) Cmd.t -> unit
