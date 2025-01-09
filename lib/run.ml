open Core

let get_out (out, _) = out
let get_err (_, err) = err

let _unchecked reader post cmd' =
  let ex, streams = Exec.in_context cmd' ~f:(fun t -> reader t) in
  ex, post streams

let _res reader post cmd =
  let ex, stream = _unchecked reader post cmd in
  Result.map (fun _ -> stream) (Exit.check ex)

let _exn reader post cmd =
  Exit.exn (_res reader post cmd)

module type S = sig
  type out
  val reader : In_channel.t -> out
end

module Make(M : S)  = struct

  let _out_helper f cmd =
    cmd |> pipe_out |> f Core.(fun t -> M.reader (stdout t), Out.conv t.stderr) get_out
  let _err_helper f cmd =
    cmd |> pipe_err |> f Core.(fun t -> Out.conv t.stdout, M.reader (stderr t)) get_err
  let _both_helper f cmd =
    cmd |> pipe_out |> pipe_err
    |> f Core.(fun t -> M.reader (stdout t), M.reader (stderr t)) Fun.id

  let res cmd = _out_helper _res cmd
  let unchecked cmd = _out_helper _unchecked cmd
  let exn cmd = _out_helper _exn cmd
  let err_unchecked cmd = _err_helper _unchecked cmd
  let err_res cmd = _err_helper _res cmd
  let err_exn cmd = _err_helper _exn cmd
  let both_unchecked cmd = _both_helper _unchecked cmd
  let both_res cmd = _both_helper _res cmd
  let both_exn cmd = _both_helper _exn cmd
end

module Read = Make(struct
    type out = string
    let reader = In_channel.input_all
  end)

module Lines = Make(struct
    type out = string list
    let reader = In_channel.input_lines
  end)

let _neither t = Core.(Out.conv t.stdout, Out.conv t.stderr)

let unchecked cmd =
  let ex, () = _unchecked _neither (fun _ -> ()) cmd in
  ex
let res cmd = _res _neither (fun _ -> ()) cmd
let exn cmd = _exn _neither (fun _ -> ()) cmd
