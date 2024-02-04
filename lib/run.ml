open Base
open Sub2

type 'out t =
  { proc: Exit.t
  ; stdout: 'out
  ; stderr: 'out
  }

module type S = sig
  type out
  val reader : In_channel.t -> out
  val empty : out
end

module Make(M : S)  = struct
  let get_reader : type o. o Out.t -> (o -> M.out) = function
    | Out.Pipe -> M.reader
    | _ -> fun _ -> M.empty

  let _unchecked cmd =
    let get_out = get_reader cmd.Cmd.stdout in
    let get_err = get_reader cmd.Cmd.stderr in
    let proc, (stdout, stderr) = in_context cmd ~f:(fun t ->
        get_out t.stdout, get_err t.stderr
      ) in {proc; stdout; stderr}

  let _res out_mapper cmd =
    let t = _unchecked cmd in
    Result.map ~f:(fun _ -> out_mapper t) (Exit.check t.proc)
    
  let _exn out_mapper cmd =
    Or_error.ok_exn (_res out_mapper cmd |> or_error)

  let _get_out t = t.stdout
  let _get_err t = t.stderr

  let _out_helper f args = cmd args |> pipe |> f
  let _err_helper f args = cmd args |> pipe_err |> f
  let _both_helper f args = cmd args |> pipe |> pipe_err |> f

  let unchecked = _out_helper _unchecked
  let res = _out_helper (_res _get_out)
  let exn = _out_helper (_exn _get_out)
  let unchecked_err = _err_helper _unchecked
  let res_err = _err_helper (_res _get_err)
  let exn_err = _err_helper (_exn _get_err)
  let unchecked_both = _both_helper _unchecked
  let res_both = _both_helper (_res Fn.id)
  let exn_both = _both_helper (_exn Fn.id)
end

module Read = Make(struct
    type out = string
    let reader = In_channel.input_all
    let empty = ""
  end)

module Lines = Make(struct
    type out = string list
    let reader = In_channel.input_lines
    let empty = []
  end)

module UnitOut = Make(struct
    type out = unit
    let reader = fun _ -> ()
    let empty = ()
  end)

let unchecked args = UnitOut._unchecked (cmd args)
let res args = UnitOut._res (fun _ -> ()) (cmd args)
let exn args = UnitOut._exn (fun _ -> ()) (cmd args)
let (let*) = (let*)
