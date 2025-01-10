let _bind_helper check cmd f =
  let proc, output = Exec.in_context cmd ~f in
  Result.bind (check proc) (fun _ -> output)

let exn cmd ~f =
  let proc, output = Exec.in_context cmd ~f in
  Result.map (fun _ -> output) (Exit.check proc)
  |> Exit.exn
  
let exit_t cmd ~f = _bind_helper Exit.check cmd f

let exit_t_both cmd ~f =
  let proc, output = Exec.shared_context ~f cmd in
  Result.bind (Exit.check proc) (fun _ -> output)

let exn_both cmd ~f =
  let proc, output = Exec.shared_context ~f cmd in
  Result.map (fun _ -> output) (Exit.check proc) 
  |> Exit.exn
