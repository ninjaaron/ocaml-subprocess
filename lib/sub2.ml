open Base
module Unix = UnixLabels
include Core

let wait ?(mode = []) t = Unix.waitpid ~mode t.pid
let poll t =
  match Unix.waitpid ~mode:[ Unix.WNOHANG ] t.pid with
  | 0, _ -> None
  | _, status -> Some status

let _create ~stdout ~stdin ~stderr args =
  if Array.length args < 1 then
    raise (Subprocess_error "process arguments can't be empty");
  Unix.create_process ~prog:args.(0) ~args ~stdout ~stdin ~stderr

let cmd args =
  Cmd.{ args
      ; stdin=In.Stdin
      ; stdout=Out.Stdout
      ; stderr=Out.Stderr
      }

let exec Cmd.{args; stdin; stdout; stderr} =
  let in' = Stream2.prep_in stdin in
  let out = Stream2.prep_out stdout in
  let err = Stream2.prep_out stderr in
  let pid = _create ~stdin:in'.send ~stdout:out.send ~stderr:err.send args in
  let close ?(mode = []) () =
    in'.closer ();
    let pid, status = Unix.waitpid ~mode pid in
    out.closer (); err.closer ();
    Exit.{pid; status; args} in
  let t = { pid
          ; args
          ; stdin = in'.handle
          ; stdout = out.handle
          ; stderr = err.handle
          ; close
          } in
  List.iter ~f:(Option.iter ~f:Unix.close) [ in'.cl; out.cl; err.cl ];
  t

let check t =
  Exit.check (t.close ())

let in_context cmd ~f =
  let t = exec cmd in
    let output =
      try f t with
      | e ->
        let _ = t.close () in
        raise e
    in
    t.close (), output

let line t = In_channel.input_line t.stdout
let lines t = In_channel.input_lines t.stdout
let err_line t = In_channel.input_line t.stderr
let err_lines t = In_channel.input_lines t.stderr

let write t s = Out_channel.output_string t.stdin s

let or_error res = Result.map_error res
    ~f:(fun err -> Error.create "non-zero status" err Exit.sexp_of_t)

let string_error res = Result.map_error res
    ~f:Exit.to_string

let _bind_helper check cmd f =
  let proc, output = in_context cmd ~f in
  Result.bind ~f:(fun _ -> output) (check proc)

let bind_exn cmd ~f =
  let proc, output = in_context cmd ~f in
  Result.map ~f:(fun _ -> output) (Exit.check proc)
  |> or_error |> Or_error.ok_exn
  
let bind_exit_t cmd ~f = _bind_helper Exit.check cmd f

let bind_or_error cmd ~f =
  _bind_helper (Fn.compose or_error Exit.check) cmd f

let bind_string_error cmd ~f =
  _bind_helper (Fn.compose string_error Exit.check) cmd f

let (let$) cmd f = bind_exit_t cmd ~f
let (let*) res f = Result.bind res ~f

module Fold = struct
  (* module type S = sig *)
  (*   type out *)
  (*   type err *)
  (*   val pipe : ('i, 'o, 'e) Cmd.t -> ('i2, out, err) Cmd.t *)
  (*   val get_stream : ('i, out, err) Cmd.t -> In_channel.t *)
  (* end *)

  (* module Make(M : S) = struct *)
  (* let unchecked cmd ~f ~init = *)
  (*   in_context (cmd |> M.pipe) ~f:(fun t -> *)
  (*       In_channel.fold_lines f init (M.get_stream t) *)
  (*     ) *)

  (* let res cmd ~f ~init = *)
  (*   let$ t = cmd |> M.pipe in *)
  (*   Ok (In_channel.fold_lines f init (M.get_stream t)) *)

  (* let exn cmd ~f ~init = *)
  (*   res cmd ~f ~init |> or_error |> Or_error.ok_exn *)
  (* end *)
  let _get_out t = t.stdout
  let _get_err t = t.stderr

  let _unchecked pipe get_stream cmd f init =
    in_context (cmd |> pipe) ~f:(fun t ->
        In_channel.fold_lines f init (get_stream t)
      )

  let _res pipe get_stream cmd f init =
    let$ t = cmd |> pipe in
    Ok (In_channel.fold_lines f init (get_stream t))

  let _exn pipe get_stream cmd f init =
    _res pipe get_stream cmd f init |> or_error |> Or_error.ok_exn

  let unchecked cmd ~f ~init = _unchecked pipe_out _get_out cmd f init
  let res cmd ~f ~init = _res pipe_out _get_out cmd f init
  let exn cmd ~f ~init = _exn pipe_out _get_out cmd f init
  let unchecked_err cmd ~f ~init = _unchecked pipe_err _get_err cmd f init
  let res_err cmd ~f ~init = _res pipe_err _get_err cmd f init
  let err cmd ~f ~init = _exn pipe_err _get_err cmd f init
end

let fold = Fold.exn
