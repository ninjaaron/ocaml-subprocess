open Core

let _unchecked pipe get_stream cmd f init =
  Exec.in_context (cmd |> pipe) ~f:(fun t ->
      In_channel.fold_lines f init (get_stream t)
    )

let _res pipe get_stream cmd f init =
  Bind.exit_t (cmd |> pipe) ~f:(fun t ->
      Ok (In_channel.fold_lines f init (get_stream t)))

let _exn pipe get_stream cmd f init =
  _res pipe get_stream cmd f init |> Exit.exn

let unchecked cmd ~f ~init = _unchecked pipe_out Core.stdout cmd f init
let res cmd ~f ~init = _res pipe_out Core.stdout cmd f init
let exn cmd ~f ~init = _exn pipe_out Core.stdout cmd f init
let unchecked_err cmd ~f ~init = _unchecked pipe_err Core.stderr cmd f init
let res_err cmd ~f ~init = _res pipe_err Core.stderr cmd f init
let err cmd ~f ~init = _exn pipe_err Core.stderr cmd f init

let unchecked_both cmd ~f ~init =
  Exec.shared_context cmd ~f:(fun t ->
      In_channel.fold_lines f init (stdout t)
    )

let res_both cmd ~f ~init =
  Bind.exit_t_both cmd ~f:(fun t ->
      Ok (In_channel.fold_lines f init (stdout t)))

let exn_both cmd ~f ~init = res_both cmd ~f ~init |> Exit.exn
