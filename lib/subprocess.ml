module Exit = Exit
module Core = Core
include Core

let exec = Bind.exn
let run = Run.exn
let read = Run.Read.exn
let lines = Run.Lines.exn
let read_err = Run.Read.err_exn
let lines_err = Run.Lines.err_exn
let fold = Fold.exn
let fold_err = Fold.err

module Results = struct
  include Core 

  let exec = Bind.exit_t
  let run = Run.res
  let read = Run.Read.res
  let lines = Run.Lines.res
  let read_err = Run.Read.err_res
  let lines_err = Run.Lines.err_res
  let fold = Fold.res
  let fold_err = Fold.res_err

  let (let*) = Result.bind
  let (let|) cmd f = exec cmd ~f

end

module Or_error = struct
  include Core

  let exec cmd ~f = Bind.or_error cmd ~f
  let run cmd = Run.res cmd |> Exit.or_error
  let read cmd = Run.Read.res cmd |> Exit.or_error
  let lines cmd = Run.Lines.res cmd |> Exit.or_error
  let read_err cmd = Run.Read.err_res cmd |> Exit.or_error
  let lines_err cmd = Run.Lines.err_res cmd |> Exit.or_error
  let fold cmd ~f ~init = Fold.res cmd ~f ~init |> Exit.or_error
  let fold_err cmd ~f ~init = Fold.res_err cmd ~f ~init |> Exit.or_error

  let (let*) = Result.bind
  let (let|) cmd f = exec cmd ~f
end

module Unchecked = struct
  include Core

  let exec = Exec.in_context
  let run = Run.unchecked
  let read = Run.Read.unchecked
  let lines = Run.Lines.unchecked
  let read_err = Run.Read.err_unchecked
  let lines_err = Run.Lines.err_unchecked
end

module Exec = Exec
