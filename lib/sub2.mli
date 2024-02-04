include module type of Core

val wait : ?mode:Unix.wait_flag list
  -> ('i, 'o, 'e) t
  -> int * Unix.process_status

val poll : ('i, 'o, 'e) t -> Unix.process_status option

val cmd : string array -> (stdin, stdout, stderr) Cmd.t

(* val set_in : 'i2 In.t -> ('i1, 'o, 'e) Cmd.t -> ('i2, 'o, 'e) Cmd.t *)
(* val set_out : 'o2 Out.t -> ('i, 'o1, 'e) Cmd.t -> ('i, 'o2, 'e) Cmd.t *)
(* val set_err : 'e2 Out.t -> ('i, 'o, 'e1) Cmd.t -> ('i, 'o, 'e2) Cmd.t *)
val pipe_in : ('i, 'o, 'e) Cmd.t -> (out_channel, 'o, 'e) Cmd.t
val pipe : ('i, 'o, 'e) Cmd.t -> ('i, in_channel, 'e) Cmd.t
val pipe_err : ('i, 'o, 'e) Cmd.t -> ('i, 'o, in_channel) Cmd.t
val channel_in : in_channel -> ('i, 'o, 'e) Cmd.t -> (channel, 'o, 'e) Cmd.t
val channel : out_channel -> ('i, 'o, 'e) Cmd.t -> ('i, channel, 'e) Cmd.t
val channel_err : out_channel -> ('i, 'o, 'e) Cmd.t -> ('i, 'o, channel) Cmd.t
val file_in : string -> ('i, 'o, 'e) Cmd.t -> (channel, 'o, 'e) Cmd.t
val file : string -> ('i, 'o, 'e) Cmd.t -> ('i, channel, 'e) Cmd.t
val file_err : string -> ('i, 'o, 'e) Cmd.t -> ('i, 'o, channel) Cmd.t
val devnull : ('i, 'o, 'e) Cmd.t -> ('i, devnull, 'e) Cmd.t
val devnull_err : ('i, 'o, 'e) Cmd.t -> ('i, 'o, devnull) Cmd.t

val check : ('i, 'o, 'e) t -> (Exit.t, Exit.t) result

val in_context : ('i, 'o, 'e) Cmd.t -> f:(('i, 'o, 'e) t -> 'a) -> Exit.t * 'a

val line : ('i, in_channel, 'e) t -> string option
val lines : ('i, in_channel, 'e) t -> string list
val err_line : ('i, 'o, in_channel) t -> string option
val err_lines : ('i, 'o, in_channel) t -> string list
val write : (out_channel, 'o, 'e) t -> string -> unit

module Fold : sig
  val unchecked : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> Exit.t * 'acc

  val res : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> ('acc, Exit.t) result

  val exn : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> 'acc

  val unchecked_err : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> Exit.t * 'acc

  val res_err : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> ('acc, Exit.t) result

  val err : ('i, 'o, 'e) Cmd.t
    -> f:('acc -> string -> 'acc)
    -> init:'acc
    -> 'acc
end

val fold : ('i, 'o, 'e) Cmd.t
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
  -> (('i, 'o, 'e) t -> ('a, Exit.t) result)
  -> ('a, Exit.t) result

val bind_or_error : ('i, 'o, 'e) Cmd.t
  -> (('i, 'o, 'e) t -> ('a, Base.Error.t) result)
  -> ('a, Base.Error.t) result

val bind_string_error : ('i, 'o, 'e) Cmd.t
  -> (('i, 'o, 'e) t -> ('a, string) result)
  -> ('a, string) result

val exec : ('i, 'o, 'e) Cmd.t -> ('i, 'o, 'e) t
