open Base

let _bind_helper check cmd f =
  let proc, output = Exec.in_context cmd ~f in
  Result.bind ~f:(fun _ -> output) (check proc)

let exn cmd ~f =
  let proc, output = Exec.in_context cmd ~f in
  Result.map ~f:(fun _ -> output) (Exit.check proc)
  |> Exit.exn
  
let exit_t cmd ~f = _bind_helper Exit.check cmd f

let or_error cmd ~f =
  _bind_helper (Fn.compose Exit.or_error Exit.check) cmd f

let string_error cmd ~f =
  _bind_helper (Fn.compose Exit.string_error Exit.check) cmd f
