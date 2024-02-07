open Base
open Core

type ('stdin, 'stdout, 'stderr) t =
  { pid: int
  ; args: string array
  ; status: Unix.process_status
  ; stdin: 'stdin
  ; stdout: 'stdout
  ; stderr: 'stderr
  }

let get_out t = t.stdout
let get_err t = t.stderr
let get_exit {pid; args; status; _} = Exit.({pid; args; status})

let _unchecked reader _ cmd =
  let Exit.{pid; args; status}, (stdin, (stdout, stderr)) =
    Exec.in_context cmd ~f:(fun t ->
      t.stdin, reader t
    ) in {pid; args; status; stdin; stdout; stderr}

let _res reader post cmd =
  let t = _unchecked reader () cmd in
  Result.map ~f:(fun _ -> post t) (Exit.check (get_exit t))

let _exn reader post cmd =
  Or_error.ok_exn (_res reader post cmd |> Bind.map_or_error)

module type S = sig
  type out
  val reader : In_channel.t -> out
end

module Make(M : S)  = struct

  let _out_helper f cmd =
    cmd |> pipe_out |> f Core.(fun t -> M.reader t.stdout, t.stderr) get_out
  let _err_helper f cmd =
    cmd |> pipe_err |> f Core.(fun t -> t.stdout, M.reader t.stderr) get_err
  let _both_helper f cmd =
    cmd |> pipe_out |> pipe_err
    |> f Core.(fun t -> M.reader t.stdout, M.reader t.stderr) Fn.id

  let unchecked cmd = _out_helper _unchecked cmd
  let res cmd = _out_helper _res cmd
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

let _neither t = Core.(t.stdout, t.stderr)

let unchecked cmd = _unchecked _neither () cmd
let res cmd = _res _neither (fun _ -> ()) cmd
let exn cmd = _exn _neither (fun _ -> ()) cmd
