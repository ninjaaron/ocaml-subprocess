open Core
module Unix = UnixLabels
open StdLabels
module type Exec_t = sig
  type 'a t
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) Core.t -> 'a) ->
    'a t
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) Core.t -> 'a) ->
    'a t
end

module type S = sig
  type 'a t
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) Core.t -> 'a) ->
    'a t
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) Core.t -> 'a) ->
    'a t
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> unit t
  val read : ('stdin, stdout, 'stderr) Cmd.t -> string t
  val lines : ('stdin, stdout, 'stderr) Cmd.t -> string list t
  val read_err : ('stdin, 'stdout, stderr) Cmd.t -> string t
  val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> string list t
  val read_joined : ('stdin, stdout, stderr) Cmd.t -> string t
  val lines_joined : ('stdin, stdout, stderr) Cmd.t -> string list t
  val read_both : ('stdin, stdout, stderr) Cmd.t -> (string * string) t
  val lines_both : ('stdin, stdout, stderr) Cmd.t
    -> (string list * string list) t
  val fold : ('stdin, stdout, 'stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
  val fold_err : ('stdin, 'stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
  val fold_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
  val fold_both : ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> (string, string) result -> 'acc) ->
    init:'acc ->
    'acc t
end

let run_f _ = ()
let read_f stream t = In_channel.input_all (stream t)
let lines_f stream t = In_channel.input_lines (stream t)
let fold_f stream f init t = In_channel.fold_lines f init (stream t)

module Make(M: Exec_t) : S with type 'a t := 'a M.t = struct
  include M
  let run cmd = exec cmd ~f:run_f
  let read cmd = exec (pipe_out cmd) ~f:(read_f stdout)
  let lines cmd = exec (pipe_out cmd) ~f:(lines_f stdout)
  let read_err cmd = exec (pipe_err cmd) ~f:(read_f stderr)
  let lines_err cmd = exec (pipe_err cmd) ~f:(lines_f stderr)
  let read_joined cmd = exec_joined cmd ~f:(read_f stdout)
  let lines_joined cmd = exec_joined cmd ~f:(lines_f stdout)
  let fold cmd ~f ~init =
    exec (pipe_out cmd) ~f:(fold_f stdout f init)
  let fold_err cmd ~f ~init =
    exec (pipe_err cmd) ~f:(fold_f stderr f init)
  let fold_joined cmd ~f ~init =
    exec_joined cmd ~f:(fold_f stdout f init)

  let bufsz = 512

  let read_both_f t =
    let buf_out = Bytes.create bufsz
    and buf_err = Bytes.create bufsz
    and collected_out = Buffer.create bufsz
    and collected_err = Buffer.create bufsz
    and out = Unix.descr_of_in_channel (stdout t)
    and err = Unix.descr_of_in_channel (stderr t) in
    let rec go = function
      | [] -> ()
      | descriptors ->
        let ready, _, _ = Unix.select
            ~read:descriptors ~write:[] ~except:[] ~timeout:(-1.) in
        let remove = List.filter ready ~f:(fun fd ->
            let buf, col = if fd = out then buf_out, collected_out
              else buf_err, collected_err in
            match Unix.read fd ~buf ~pos:0 ~len:bufsz with
            | 0 -> true 
            | n ->
              Buffer.add_subbytes col buf 0 n;
              false) in
        let descrs = List.filter descriptors ~f:(fun fd ->
            not (List.mem fd ~set:remove)) in
        go descrs in
    go [out; err];
    Buffer.(contents collected_out, contents collected_err)

  let read_both cmd = exec ~f:read_both_f @@ pipe_out @@ pipe_err cmd

  let lines_both cmd =
    let cmd' = pipe_out @@ pipe_err cmd in
    exec cmd' ~f:(fun t ->
        let out, err = read_both_f t in
        String.(split_on_char ~sep:'\n' out, split_on_char ~sep:'\n' err))

  let fold_both cmd ~f ~init =
    let cmd' = no_block @@ pipe_out @@ pipe_err cmd in
    exec cmd' ~f:(fun t ->
        let rec go ?(finished=false) acc stream other =
          match finished, In_channel.input_line stream with
          | exception Sys_blocked_io ->
            Unix.sleepf 0.01;
            if finished
            then go ~finished acc stream other
            else go acc other stream
          | true, None -> acc
          | false, None -> go ~finished:true acc other stream
          | _, Some line ->
            let acc' = if stream == (stdout t)
              then f acc (Ok line) else f acc (Error line) in
            if finished
            then go ~finished acc' stream other
            else go acc' other stream in
        go init (stdout t) (stderr t))
end
