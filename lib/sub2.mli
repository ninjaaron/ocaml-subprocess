include module type of Core

val wait : ?mode:Unix.wait_flag list
  -> ('i, 'o, 'e) t
  -> int * Unix.process_status

val poll : ('i, 'o, 'e) t -> Unix.process_status option

val cmd : string array -> (stdin, stdout, stderr) Cmd.t

val check : ('i, 'o, 'e) t -> (Exit.t, Exit.t) result

val in_context : ('i, 'o, 'e) Cmd.t -> f:(('i, 'o, 'e) t -> 'a) -> Exit.t * 'a

val line : ('i, in_channel, 'e) t -> string option
val lines : ('i, in_channel, 'e) t -> string list
val err_line : ('i, 'o, in_channel) t -> string option
val err_lines : ('i, 'o, in_channel) t -> string list
val write : (out_channel, 'o, 'e) t -> string -> unit

module Fold : sig
  val unchecked : ('i, stdout, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> Exit.t * 'acc

  val res : ('i, stdout, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> ('acc, Exit.t) result

  val exn : ('i, stdout, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> 'acc

  val unchecked_err : ('i, 'o, stderr) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> Exit.t * 'acc

  val res_err : ('i, 'o, stderr) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> ('acc, Exit.t) result

  val err : ('i, 'o, stderr) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> 'acc
end

val fold : ('i, stdout, 'e) Cmd.t
  -> f:('acc -> string -> 'acc)
  -> init:'acc
  -> 'acc
    
val or_error : ('a, Exit.t) result -> ('a, Base.Error.t) result
val string_error : ('a, Exit.t) result -> ('a, string) result

val (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val (let$) : ('i, 'o, 'e) Cmd.t
  -> (('i, 'o, 'e) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val bind_exit_t : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val bind_or_error : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, Base.Error.t) result)
  -> ('a, Base.Error.t) result

val bind_string_error : ('i, 'o, 'e) Cmd.t
  -> f:(('i, 'o, 'e) t -> ('a, string) result)
  -> ('a, string) result

val bind_exn : ('i, 'o, 'e) Cmd.t -> f:(('i, 'o, 'e) t -> 'a) -> 'a

val exec : ('i, 'o, 'e) Cmd.t -> ('i, 'o, 'e) t
