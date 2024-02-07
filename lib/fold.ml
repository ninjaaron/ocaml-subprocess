open Core
open Base
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
  Exec.in_context (cmd |> pipe) ~f:(fun t ->
      In_channel.fold_lines f init (get_stream t)
    )

let _res pipe get_stream cmd f init =
  Bind.exit_t (cmd |> pipe) ~f:(fun t ->
  Ok (In_channel.fold_lines f init (get_stream t)))

let _exn pipe get_stream cmd f init =
  _res pipe get_stream cmd f init |> Bind.map_or_error |> Or_error.ok_exn

let unchecked cmd ~f ~init = _unchecked pipe_out _get_out cmd f init
let res cmd ~f ~init = _res pipe_out _get_out cmd f init
let exn cmd ~f ~init = _exn pipe_out _get_out cmd f init
let unchecked_err cmd ~f ~init = _unchecked pipe_err _get_err cmd f init
let res_err cmd ~f ~init = _res pipe_err _get_err cmd f init
let err cmd ~f ~init = _exn pipe_err _get_err cmd f init
