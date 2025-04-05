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
  val write : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string -> unit t
  val write_lines : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string Seq.t -> unit t
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
  val fold_both : ?sleep:float ->
    ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> (string, string) result -> 'acc) ->
    init:'acc ->
    'acc t

  val fold_with : ?sep:string ->
    (stdin, stdout, 'stderr) Cmd.t ->
    lines:string Seq.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t
end

let run_f _ = ()
let read_f stream t = In_channel.input_all (stream t)
let lines_f stream t = In_channel.input_lines (stream t)
let fold_f stream f init t = In_channel.fold_lines f init (stream t)

let bufsz = 512

let read_both_proc t =
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

let fold_both_proc ?(sleep=0.) t ~f ~init =
  let get_res stream line =
    if stream == (stdout t) then (Ok line) else (Error line) in
  let rec go acc stream other =
    match In_channel.input_line stream with
    | exception Sys_blocked_io ->
      if sleep > 0. then Unix.sleepf sleep;
      go acc other stream
    | None -> go_one acc other
    | Some line -> go (f acc (get_res stream line)) other stream
  and go_one acc stream =
    match In_channel.input_line stream with
    | exception Sys_blocked_io -> Unix.sleepf sleep; go_one acc stream
    | None -> acc
    | Some line -> go_one (f acc (get_res stream line)) stream in
  go init (stdout t) (stderr t)

let fold_with_proc ?(sep="\n") t ~lines ~f ~init =
  let write_line line lines =
    match Out_channel.output_string (stdin t) line with
    | exception Sys_blocked_io -> Seq.cons line lines
    | () ->
      match Out_channel.output_string (stdin t) sep with
      | exception Sys_blocked_io -> Seq.cons "" lines
      | () -> lines in
  let rec go lines_opt acc =
    let lines_opt' = Option.bind lines_opt @@ fun lines ->
      match lines () with
      | Seq.Nil -> Out_channel.close (stdin t); None
      | Seq.Cons (line, tl) -> Some (write_line line tl) in
    match In_channel.input_line (stdout t) with
    | exception Sys_blocked_io -> go lines_opt' acc
    | None -> acc
    | Some line ->
      go lines_opt' (f acc line) in
  go (Some lines) init

module Make(M: Exec_t) : S with type 'a t := 'a M.t = struct
  include M
  let run cmd = exec cmd ~f:run_f

  let write cmd ~input =
    exec (pipe_in cmd) ~f:(fun t ->
        Out_channel.output_string (stdin t) input)

  let write_lines cmd ~input =
    exec (pipe_in cmd) ~f:(fun t ->
        set_binary_mode_out (stdin t) false;
        Seq.iter (fun line ->
            Out_channel.output_string (stdin t) line;
            Out_channel.output_char (stdin t) '\n'
          ) input
      )
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

  let read_both cmd = exec ~f:read_both_proc @@ pipe_out @@ pipe_err cmd

  let remove_trailing_newline s =
    let len = String.length s in
    if s.[len-1] = '\n' then String.sub ~pos:0 ~len:(len-1) s
    else s

  let lines_both cmd =
    let f t =
      let out, err = read_both_proc t in
      String.(
        split_on_char ~sep:'\n' (remove_trailing_newline out),
        split_on_char ~sep:'\n' (remove_trailing_newline err)) in
    let cmd' = pipe_out @@ pipe_err cmd in
    exec cmd' ~f

  let fold_both ?sleep cmd ~f ~init =
    let cmd' = no_block @@ pipe_out @@ pipe_err cmd in
    exec cmd' ~f:(fold_both_proc ?sleep ~f ~init)

  let fold_with ?sep cmd ~lines ~f ~init =
    exec (cmd |> pipe_in |> pipe_out |> no_block)
      ~f:(fold_with_proc ?sep ~lines ~f ~init)
end
