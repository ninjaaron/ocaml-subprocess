module Unix = UnixLabels
open Core
open StdLabels

module Smap = Map.Make(String)
module Omap = Map.Make(struct
    type t = Cmd.Out.poly
    let compare = Cmd.Out.compare
  end)

type fd = Unix.file_descr

module Stream = struct
  let get_devnull () =
    Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY] ~perm:0

  type 'a bookkeeping =
    { handle: 'a
    ; send: Unix.file_descr
    ; cl: Unix.file_descr option
    ; closer: unit -> unit
    }

  let prep_fd_no_close fd handle =
    { handle
    ; send = fd
    ; cl = None
    ; closer = Fun.id
    }

  let prep_fd_close fd handle =
    { handle
    ; send = fd
    ; cl = Some fd
    ; closer = Fun.id
    }

  let prep_out (type a) : block:bool -> a Cmd.Out.t -> a Out.t bookkeeping =
    fun ~block -> function
    | Stdout -> prep_fd_no_close Unix.stdout Out.Stdout
    | Stderr -> prep_fd_no_close Unix.stderr Out.Stderr
    | Devnull -> prep_fd_close (get_devnull ()) Out.Devnull
    | Channel oc ->
      let fd = Unix.descr_of_out_channel oc in
      prep_fd_no_close fd Out.Channel
    | File s ->
      let fd = Unix.(openfile s ~mode:[O_WRONLY; O_CREAT] ~perm:0o640) in
      prep_fd_close fd (Out.File s)
    | Append s ->
      let fd = Unix.(openfile s ~mode:[O_APPEND; O_CREAT] ~perm:0o640) in
      prep_fd_close fd (Out.Append s)
    | Pipe ->
      let r, w = Unix.pipe ~cloexec:true () in
      if not block then Unix.set_nonblock r;
      let ic = Unix.in_channel_of_descr r in
      { handle = Out.Pipe ic
      ; send = w
      ; cl = Some w
      ; closer = fun () -> In_channel.close_noerr ic
      }

  let prep_out_pipeline
    : type a. ?reader:in_channel
      -> fd_map:fd Omap.t
      -> block:bool
      -> is_stdout:bool
      -> a Cmd.Out.t
      -> a Out.t bookkeeping * in_channel option * fd Omap.t
    = fun ?reader ~fd_map ~block ~is_stdout out_t ->
      let prep = prep_fd_no_close in
      let out_poly = Cmd.Out.Out out_t in
      let add fd = Omap.add out_poly fd fd_map in
      match out_t, Omap.find_opt out_poly fd_map, reader with
      | Stdout, _, _ -> prep Unix.stdout Out.Stdout, reader, fd_map
      | Stderr, _, _ -> prep Unix.stderr Out.Stderr, reader, fd_map
      | Channel oc, _, _ ->
        prep (Unix.descr_of_out_channel oc) Out.Channel, reader, fd_map
      (* now all the types were the fd needs to be managed *)
      | Devnull, Some fd, _ -> prep_fd_no_close fd Out.Devnull, reader, fd_map
      | Devnull, None, _ -> let fd = get_devnull () in
        prep fd Out.Devnull, reader, add fd
      | File s, Some fd, _ -> prep fd (Out.File s), reader, fd_map
      | File s, None, _ ->
        let fd = Unix.(openfile s ~mode:[O_WRONLY; O_CREAT] ~perm:0o640) in
        prep fd (Out.File s), reader, add fd
      | Append s, Some fd, _ -> prep fd (Out.Append s), reader, fd_map
      | Append s, None, _ ->
        let fd = Unix.(openfile s ~mode:[O_APPEND; O_CREAT] ~perm:0o640) in
        prep fd (Out.Append s), reader, add fd
      | Pipe, _, _ when is_stdout ->
        let r, w = Unix.pipe ~cloexec:true () in
        if not block then Unix.set_nonblock r;
        let ic = Unix.in_channel_of_descr r in
        { handle = Out.Pipe ic
        ; send = w
        ; cl = Some w
        ; closer = fun () -> In_channel.close_noerr ic
        }, reader, fd_map
      | Pipe, Some send, Some ic ->
        {handle=Out.Pipe ic; send; cl=None; closer=Fun.id}, reader, fd_map
      | Pipe, None, None ->
        let r, send = Unix.pipe ~cloexec:true () in
        if not block then Unix.set_nonblock r;
        let ic = Unix.in_channel_of_descr r in
        ( {handle=Out.Pipe ic; send ; cl=None ; closer=Fun.id}
        , Some ic
        , add send
        )
      | Pipe, _, _ -> assert false

  let prep_in (type a) : block:bool -> a Cmd.In.t -> a In.t bookkeeping =
    fun ~block -> function
    | Stdin -> prep_fd_no_close Unix.stdin In.Stdin
    | Channel ic ->
      let fd = Unix.descr_of_in_channel ic in
      prep_fd_no_close fd In.Channel
    | File s ->
      (match Unix.(openfile s ~mode:[O_RDONLY] ~perm:0) with
       | exception Unix.(Unix_error (ENOENT, _, _)) ->
         raise @@ Subprocess_error (s ^ ": no such file or directory")
       | fd -> prep_fd_close fd (In.File s))
    | Pipe ->
      let r, w = Unix.pipe ~cloexec:true () in
      if not block then Unix.set_nonblock w;
      let oc = Unix.out_channel_of_descr w in
      { handle = In.Pipe oc
      ; send = r
      ; cl = Some r
      ; closer = fun () -> Out_channel.close_noerr oc
      }
end

let env_join env =
  let passed = ref @@ Smap.of_list env in
  Array.append
    (Array.map (Unix.environment ()) ~f:(fun pair ->
         match String.index_opt pair '=' with
         | None -> pair
         | Some len ->
           let key = String.sub ~pos:0 ~len pair in
           match Smap.find_opt key !passed with
           | None -> pair
           | Some value ->
             passed := Smap.remove key !passed;
             Printf.sprintf "%s=%s" key value))
    (Smap.to_seq !passed
     |> Seq.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
     |> Array.of_seq)

let _create ~stdout ~stdin ~stderr ~env (prog, args) =
  if env = [] then
    Unix.create_process ~prog ~args ~stdout ~stdin ~stderr
  else
    Unix.create_process_env
      ~prog ~env:(env_join env) ~args ~stdout ~stdin ~stderr

let rec waitpid ~mode pid =
  try Unix.waitpid ~mode pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid ~mode pid

let exec (Cmd.{args; stdin; stdout; stderr; env; block} as cmd) =
  let in', out, err =
    Stream.( prep_in ~block stdin
           , prep_out ~block stdout
           , prep_out ~block stderr
           ) in
  let pid = _create
      ~stdin:in'.send
      ~stdout:out.send
      ~stderr:err.send
      ~env
      args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.(Exit {pid; status; cmd}) in
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  { pid
  ; cmd
  ; stdin = in'.handle
  ; stdout = out.handle
  ; stderr = err.handle
  ; close
  }

let exec_in_pipeline
    ?reader
    ~fd_map
    (Cmd.{args; stdin; stdout; stderr; env; block} as cmd)
  =
  let in', (out, _, _), (err, reader', fd_map') =
    Stream.( prep_in ~block stdin
           , prep_out_pipeline ~fd_map ~block ~is_stdout:true stdout
           , prep_out_pipeline ?reader ~fd_map ~block ~is_stdout:false stderr
           ) in
  let pid = _create
      ~stdin:in'.send
      ~stdout:out.send
      ~stderr:err.send
      ~env
      args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = waitpid ~mode pid in
    out.closer ();
    Exit.(Exit {pid; status; cmd}) in
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl; err.cl ];
  { pid
  ; cmd
  ; stdin = in'.handle
  ; stdout = out.handle
  ; stderr = err.handle
  ; close
  }, reader', fd_map'

let in_context cmd ~f =
  let t = exec cmd in
  match f t with
  | output -> t.close (), output
  | exception e ->
    let _ = t.close () in
    raise e

let shared_pipe (Cmd.{args; stdin; env; block; _} as cmd) =
  let in', out, err =
    let out = Stream.prep_out ~block Pipe in
    Stream.prep_in ~block stdin, out, out in
  let pid = _create
      ~stdin:in'.send
      ~stdout:out.send
      ~stderr:err.send
      ~env
      args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = Unix.waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.(Exit {pid; status; cmd}) in
  List.iter ~f:(Option.iter Unix.close) [ in'.cl; out.cl ];
  { pid
  ; cmd={cmd with stdout=Pipe; stderr=Stdout}
  ; stdin = in'.handle
  ; stdout = out.handle
  ; stderr = Stdout
  ; close
  }

let shared_context cmd ~f =
  let t = shared_pipe cmd in
  match f t with
  | output -> t.close (), output
  | exception e ->
    let _ = t.close () in
    raise e
