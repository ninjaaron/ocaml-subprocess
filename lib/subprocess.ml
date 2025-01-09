module Exit = Exit
module Core = Core
module Cmd = Cmd
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
  let (let&) cmd f = exec cmd ~f

end

module Unchecked = struct
  include Core

  let exec = Exec.in_context
  let run = Run.unchecked
  let read = Run.Read.unchecked
  let lines = Run.Lines.unchecked
  let read_err = Run.Read.err_unchecked
  let lines_err = Run.Lines.err_unchecked
  let fold = Fold.unchecked
  let fold_err = Fold.unchecked_err
end

module Exec = Exec
