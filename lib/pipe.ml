open Core

module Cmd = struct
  type (_, _, _) t =
    | Single : ('i, 'o, 'e) Cmd.t -> ('i, 'o, 'e) t
    | Pipe : ('i, stdout, _) Cmd.t * (stdin, 'o, 'e) t -> ('i, 'o, 'e) t
end

module C = Cmd


let cmd ?prog ?env ?block args = C.Single (Core.cmd ?prog ?env ?block args)

let [@tail_mod_cons] rec append
  : type i o e. (i, stdout, _) C.t -> (stdin, o, e) C.t -> (i, o, e) C.t =
  fun t1 t2 ->
  match t1 with
  | Single cmd -> Pipe (cmd, t2)
  | Pipe (cmd, tl) -> Pipe(cmd, append tl t2)

let (@|) = append

let set_in
  : type o e. 'i Core.Cmd.In.t  -> (stdin, o, e) C.t -> ('i, o, e) C.t =
  fun in_t -> function
  | Single cmd -> Single Core.Cmd.{cmd with stdin=in_t}
  | Pipe (cmd, t) -> Pipe (Core.Cmd.{cmd with stdin=in_t}, t)

let rec set_out
  : type i e. 'o Core.Cmd.Out.t -> (i, stdout, e) C.t -> (i, 'o, e) C.t =
  fun out_t -> function
  | Single cmd -> Single Core.Cmd.{cmd with stdout=out_t}
  | Pipe (cmd, t) -> Pipe (cmd, set_out out_t t)

let rec set_err
  : type i o. 'e Core.Cmd.Out.t -> (i, o, stderr) C.t -> (i, o, 'e) C.t =
  fun out_t -> function
  | Single cmd -> Single Core.Cmd.{cmd with stderr = out_t}
  | Pipe (cmd, t) -> Pipe (cmd, set_err out_t t)

let pipe_in t = set_in Pipe t
let pipe_out t = set_out Pipe t
let pipe_err t = set_err Pipe t
let channel_in ic t = set_in (Channel ic) t
let channel_out oc t = set_out (Channel oc) t
let channel_err oc t = set_err (Channel oc) t
let file_in s t = set_in (File s) t
let file_out s t = set_out (File s) t
let file_err s t = set_err (File s) t
let append_out s t = set_out (Append s) t
let append_err s t = set_err (Append s) t
let devnull_out t = set_out Devnull t
let devnull_err t = set_err Devnull t

let rec env : type i o e. string list -> (i, o, e) C.t -> (i, o, e) C.t =
  fun env_list -> function
    | Single cmd -> Single (Core.env env_list cmd)
    | Pipe (t1, t2) -> Pipe (Core.env env_list t1, env env_list t2)

let no_block t =
  let rec unblock_output : type i o e. (i, o, e) C.t -> (i, o, e) C.t =
    function
    | Single cmd -> Single (no_block cmd)
    | Pipe (t1, t2) -> Pipe(t1, unblock_output t2) in
  match t with
  | C.Single cmd -> C.Single (no_block cmd)
  | Pipe (cmd, t) -> Pipe (no_block cmd, unblock_output t)

module T = struct
  type (_, _, _) t =
    | Single : ('i, 'o, 'e) Core.t -> ('i, 'o, 'e) t
    | Pipe : ('i, pipe, _) Core.t * (channel, 'o, 'e) t -> ('i, 'o, 'e) t
end

include T

let stdin = function
  | Single proc -> Core.stdin proc
  | Pipe (t, _) -> stdin t

let rec stdout : type i e. (i, pipe, e) t -> in_channel =
  function
  | Single proc -> Core.stdout proc
  | Pipe (_, t) -> stdout t

let rec stderr : type i o. (i, o, pipe) t -> in_channel =
  function
  | Single proc -> Core.stderr proc
  | Pipe (_, t) -> stderr t


module Exec = struct

  let rec close
    : type i o e. ?mode:Unix.wait_flag list -> (i, o, e) t-> Exit.t =
    fun ?mode -> function
    | Single proc -> proc.close ?mode ()
    | Pipe (proc, t2) ->
      let e = proc.close ?mode () in
      match Exit.status_int e with
      | 0 -> close ?mode t2
      | _ -> ignore (close ?mode t2); e
  let _ = Exec.exec

  let exec tt =
    let rec loop : type o e. in_channel -> (stdin, o, e) C.t -> (channel, o, e) t =
      fun ic -> function
        | Single cmd -> Single (Exec.exec @@ Core.channel_in ic cmd)
        | Pipe (cmd, t) ->
          let proc = Exec.exec @@ Core.channel_in ic @@ Core.pipe_out cmd in
          Pipe(proc, loop (Core.stdout proc) t) in
    match tt with
    | Cmd.Single cmd -> Single (Exec.exec cmd)
    | Pipe (cmd, p2) ->
      let proc = Exec.exec (Core.pipe_out cmd) in
      let out = Core.stdout proc in
      Unix.(clear_nonblock @@ descr_of_in_channel out);
      Pipe (proc, loop out p2)

  let in_context tt ~f =
    let t = exec tt in
    match f t with
    | output -> close t, output
    | exception e ->
      let _ = close t in
      raise e

  let shared_pipe tt =
    let rec loop : in_channel -> (stdin, stdout, stderr) C.t -> (channel, pipe, stdout) t =
      fun ic -> function
        | Single cmd -> Single (Exec.shared_pipe @@ Core.channel_in ic cmd)
        | Pipe (cmd, t) ->
          let proc = Exec.exec @@ Core.channel_in ic @@ Core.pipe_out cmd in
          Pipe(proc, loop (Core.stdout proc) t) in
    match tt with
    | C.Single cmd -> Single (Exec.shared_pipe cmd)
    | Pipe (cmd, p2) ->
      let proc = Exec.exec (Core.pipe_out cmd) in
      let out = Core.stdout proc in
      Unix.(clear_nonblock @@ descr_of_in_channel out);
      Pipe (proc, loop out p2)

  let shared_context tt ~f =
    let t = shared_pipe tt in
    match f t with
    | output -> close t, output
    | exception e ->
      let _ = close t in
      raise e

end

module Unix = UnixLabels
open StdLabels
module type Exec_t = sig
  type 'a t

  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) T.t -> 'a) ->
    'a t
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) T.t -> 'a) ->
    'a t
end

module type S = sig
  type 'a t

  (** Execute a command in, where a handle to the created process will
      be availbe as the sole input parameter of the function [f]. When
      [f] exits, we close any "dangling" file descriptors and wait for
      the process to exit, finally returning the output value of [f]
      wrapped in the output type.  *)
  val exec : ('stdin, 'stdout, 'stderr) Cmd.t ->
    f:(('stdin, 'stdout, 'stderr) T.t -> 'a) ->
    'a t

  (** Same as {!exec}, but stdout and stderr are redirected to the
      same pipe. This is similar to [2>&1 |] in the shell.  *)
  val exec_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:(('stdin, pipe, stdout) T.t -> 'a) ->
    'a t

  (** Execute the command and wait for it to exit, returning [()]
      wrapped in the output type.  *)
  val run : ('stdin, 'stdout, 'stderr) Cmd.t -> unit t

  (** Execute the command and write the [input] string to the stdin of
      the process.  *)
  val write : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string -> unit t

  (** Execute the command and write the [input] [Seq.t] instance to
      the stdin of the process, separated by newline characters. If
      you want to read from stdout while writing, use
      {!fold_with}.  *)
  val write_lines : (stdin, 'stdout, 'stderr) Cmd.t ->
    input:string Seq.t -> unit t

  (** Execute the command and read its stdout into a string wrapped in
      the output type.  *)
  val read : ('stdin, stdout, 'stderr) Cmd.t -> string t

  (** Same as {!read} but with a list of lines wrapped in the output
     type.  *)
  val lines : ('stdin, stdout, 'stderr) Cmd.t -> string list t

  (** Same as {!read}, but reads stderr rather than stdout. *)
  val read_err : ('stdin, 'stdout, stderr) Cmd.t -> string t

  (** Same as {!lines}, but reads stderr rather than stdout. *)
  val lines_err : ('stdin, 'stdout, stderr) Cmd.t -> string list t

  (** Same as {!read} but reads stdout and stderr as a single
      stream.  *)
  val read_joined : ('stdin, stdout, stderr) Cmd.t -> string t

  (** Same as {!lines} but reads stdout and stderr as a single
      stream.  *)
  val lines_joined : ('stdin, stdout, stderr) Cmd.t -> string list t

  (** Same as {!read_joined} but reads stdout and stderr as a separate
      strings streams, returning a pair of strings wrapped in the
      ouput type. This function uses asynchronous I/O internally, so the
      optional [sleep] parameter is provided as a means to pause briefly
      if output is expected to be slow to avoid pegging the CPU with a
      loop that does nothing. *)
  val read_both :
    ?sleep:float ->
    ('stdin, stdout, stderr) Cmd.t ->
    (string * string) t

  (** Same as {!lines_joined} but reads stdout and stderr as a separate
      strings streams, returning a pair of string lists wrapped in the
      ouput type. This function uses asynchronous I/O internally, so the
      optional [sleep] parameter is provided as a means to pause briefly
      if output is expected to be slow to avoid pegging the CPU with a
      loop that does nothing. *)
  val lines_both :
    ?sleep:float ->
    ('stdin, stdout, stderr) Cmd.t ->
    (string list * string list) t

  (** Execute the command. Do a left fold over the lines of output
      from the processe's stdout. Wraps the accumulated output in the
      output type.  *)
  val fold : ('stdin, stdout, 'stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t

  (** Same as {!fold}, but fold over lines from stderr. *)
  val fold_err : ('stdin, 'stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t

  (** Same as {!fold}, but joins stdout and stderr into a single
      stream.  *)
  val fold_joined : ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> string -> 'acc) ->
    init:'acc ->
    'acc t

  (** Same as {!fold_joined}, but wraps each line of stdin in [Ok]
      and lines of stderr in [Error] so they can be
      distinguished. This function uses asynchronous I/O internally,
      so the optional [sleep] parameter is provided as a means to
      pause briefly if output is expected to be slow to avoid pegging
      the CPU with a loop that does nothing. *)
  val fold_both : ?sleep:float ->
    ('stdin, stdout, stderr) Cmd.t ->
    f:('acc -> (string, string) result -> 'acc) ->
    init:'acc ->
    'acc t

  (** Execute the command. Feed items from [lines] to the process
      stdin, which will be separated with a newline character by
      default. Use the optional [sep] parameter to change this to
      another string. While all that is happening, fold over lines
      from stdout. This function uses asynchronous I/O internally, so
      the optional [sleep] parameter is provided as a means to pause
      briefly if output is expected to be slow to avoid pegging the
      CPU with a loop that does nothing. *)
  val fold_with : ?sleep:float ->
    ?sep:string ->
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

let bufsz =  128

let read_both_proc ?(sleep=0.) t =
  let buf_out = Bytes.create bufsz
  and buf_err = Bytes.create bufsz
  and collected_out = Buffer.create bufsz
  and collected_err = Buffer.create bufsz
  and out = stdout t
  and err = stderr t in
  let rec go (chan, buf, col) other =
    match In_channel.input chan buf 0 bufsz with
    | exception Sys_blocked_io ->
      if sleep > 0. then Unix.sleepf sleep;
      go other (chan, buf, col)
    | 0 -> go_one other
    | n ->
      Buffer.add_subbytes col buf 0 n;
      go other (chan, buf, col)
  and go_one (chan, buf, col) =
    match In_channel.input chan buf 0 bufsz with
    | exception Sys_blocked_io ->
      if sleep > 0. then Unix.sleepf sleep;
      go_one (chan, buf, col)
    | 0 -> ()
    | n ->
      Buffer.add_subbytes col buf 0 n;
      go_one (chan, buf, col) in
  go (out, buf_out, collected_out) (err, buf_err, collected_err);
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

let fold_with_proc ?(sleep=0.) ?(sep="\n") t ~lines ~f ~init =
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
    | None ->
      if sleep > 0. then (Unix.sleepf sleep; acc)
      else acc
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

  let read_both ?sleep cmd =
    exec ~f:(read_both_proc ?sleep) @@ pipe_out @@ pipe_err cmd

  let remove_trailing_newline s =
    let len = String.length s in
    if s.[len-1] = '\n' then String.sub ~pos:0 ~len:(len-1) s
    else s

  let lines_both ?sleep cmd =
    let f t =
      let out, err = read_both_proc ?sleep t in
      String.(
        split_on_char ~sep:'\n' (remove_trailing_newline out),
        split_on_char ~sep:'\n' (remove_trailing_newline err)) in
    let cmd' = pipe_out @@ pipe_err cmd in
    exec cmd' ~f

  let fold_both ?sleep cmd ~f ~init =
    let cmd' = no_block @@ pipe_out @@ pipe_err cmd in
    exec cmd' ~f:(fold_both_proc ?sleep ~f ~init)

  let fold_with ?sleep ?sep cmd ~lines ~f ~init =
    exec (cmd |> pipe_in |> pipe_out |> no_block)
      ~f:(fold_with_proc ?sleep ?sep ~lines ~f ~init)
end
