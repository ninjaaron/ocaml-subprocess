open Base

let map_or_error res = Result.map_error res
    ~f:(fun err -> Error.create "non-zero status" err Exit.sexp_of_t)

let map_string_error res = Result.map_error res
    ~f:Exit.to_string

let _bind_helper check cmd f =
  let proc, output = Exec.in_context cmd ~f in
  Result.bind ~f:(fun _ -> output) (check proc)

let exn cmd ~f =
  let proc, output = Exec.in_context cmd ~f in
  Result.map ~f:(fun _ -> output) (Exit.check proc)
  |> map_or_error |> Or_error.ok_exn
  
let exit_t cmd ~f = _bind_helper Exit.check cmd f

let or_error cmd ~f =
  _bind_helper (Fn.compose map_or_error Exit.check) cmd f

let string_error cmd ~f =
  _bind_helper (Fn.compose map_string_error Exit.check) cmd f
