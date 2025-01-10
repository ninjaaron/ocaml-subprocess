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
let exec_both = Bind.exn_both
let read_both = Run.Read.both_exn
let lines_both = Run.Lines.both_exn
let fold = Fold.exn
let fold_err = Fold.err
let fold_both = Fold.exn_both

let (let&) cmd f = exec cmd ~f

module Results = struct
  include Core 

  let exec = Bind.exit_t
  let run = Run.res
  let read = Run.Read.res
  let lines = Run.Lines.res
  let read_err = Run.Read.err_res
  let lines_err = Run.Lines.err_res
  let exec_both = Bind.exit_t_both
  let read_both = Run.Read.both_res
  let lines_both = Run.Lines.both_res
  let fold = Fold.res
  let fold_err = Fold.res_err
  let fold_both = Fold.res_both

  let string_error = Exit.string_error

  let (let*) = Result.bind
  let (let&) cmd f = exec cmd ~f

end

module Unchecked = struct
  include Core

  let exec = Exec.in_context
  let exec_both = Exec.shared_context
  let run = Run.unchecked
  let read = Run.Read.unchecked
  let lines = Run.Lines.unchecked
  let read_err = Run.Read.err_unchecked
  let lines_err = Run.Lines.err_unchecked
  let read_both = Run.Read.both_unchecked
  let lines_both = Run.Lines.both_unchecked
  let fold = Fold.unchecked
  let fold_err = Fold.unchecked_err
  let fold_both = Fold.unchecked_both
end

module Exec = Exec
