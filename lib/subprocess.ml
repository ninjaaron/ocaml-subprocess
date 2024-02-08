include Core
module Exit = Exit

let exec = Bind.exn
let run = Run.exn
let read = Run.Read.exn
let lines = Run.Lines.exn
let read_err = Run.Read.err_exn
let lines_err = Run.Lines.err_exn

module Results = struct

  let exec = Bind.exit_t
  let run = Run.res
  let read = Run.Read.res
  let lines = Run.Lines.res
  let read_err = Run.Read.res
  let lines_err = Run.Lines.res

  let (let*) = Result.bind
  let (let$) cmd f = exec cmd ~f

  include Core
end

module Or_error = struct
  let exec cmd ~f = Bind.exit_t cmd ~f |> Exit.or_error
  let run cmd = Run.res cmd |> Exit.or_error
  let read cmd = Run.Read.res cmd |> Exit.or_error
  let lines cmd = Run.Lines.res cmd |> Exit.or_error
  let read_err cmd = Run.Read.res cmd |> Exit.or_error
  let lines_err cmd = Run.Lines.res cmd |> Exit.or_error

  let (let*) = Result.bind
  let (let$) cmd f = exec cmd ~f

  include Core
end

module Unchecked = struct
  let exec = Exec.in_context
  let run = Run.unchecked
  let read = Run.Read.unchecked
  let lines = Run.Lines.unchecked
  let read_err = Run.Read.err_unchecked
  let lines_err = Run.Lines.err_unchecked

  include Core
end

module Exec = Exec
