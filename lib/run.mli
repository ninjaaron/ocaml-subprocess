open Sub2
type ('out, 'err) t = { proc : Exit.t; stdout : 'out; stderr : 'err; }

module Read : sig
  val unchecked : ('i, stdout, 'e) Cmd.t -> (string, 'e) t
  val res : ('i, stdout, 'e) Cmd.t -> (string, Exit.t) result
  val exn : ('i, stdout, 'e) Cmd.t -> string
  val err_unchecked : ('i, 'o, stderr) Cmd.t -> ('o, string) t
  val err_res : ('i, 'o, stderr) Cmd.t -> (string, Exit.t) result
  val err_exn : ('i, 'o, stderr) Cmd.t -> string
  val both_unchecked : ('i, stdout, stderr) Cmd.t -> (string, string) t
  val both_res :
    ('i, stdout, stderr) Cmd.t -> ((string, string) t, Exit.t) result
  val both_exn : ('i, stdout, stderr) Cmd.t -> (string, string) t
end

module Lines : sig
  val unchecked : ('i, stdout, 'e) Cmd.t -> (string list, 'e) t
  val res : ('i, stdout, 'e) Cmd.t -> (string list, Exit.t) result
  val exn : ('i, stdout, 'e) Cmd.t -> string list
  val err_unchecked : ('i, 'o, stderr) Cmd.t -> ('o, string list) t
  val err_res : ('i, 'o, stderr) Cmd.t -> (string list, Exit.t) result
  val err_exn : ('i, 'o, stderr) Cmd.t -> string list
  val both_unchecked : ('i, stdout, stderr) Cmd.t -> (string list, string list) t
  val both_res : ('i, stdout, stderr) Cmd.t -> ((string list, string list) t, Exit.t) result
  val both_exn : ('i, stdout, stderr) Cmd.t -> (string list, string list) t
end

val unchecked : ('i, 'o, 'e) Cmd.t -> ('o, 'e) t
val res : ('i, 'o, 'e) Cmd.t -> (unit, Exit.t) result
val exn : ('i, 'o, 'e) Cmd.t -> unit
