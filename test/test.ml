let with_shell args =
  Subprocess.cmd ["sh"; "-c"; args]

let print_to_stdout = with_shell "echo 'foo\nbar\nbaz'"
let print_to_stderr = with_shell ">&2 echo 'foo\nbar\nbaz'"
let non_zero = Subprocess.cmd ["false"]
let error_with_output = with_shell "echo 'message'; false"

module TestExceptionsInterface = struct

  open Subprocess

  let%expect_test "echo" =
    run print_to_stdout;
    [%expect {|
      foo
      bar
      baz |}]

  let%expect_test "error message" =
    run print_to_stderr;
    [%expect {|
      foo
      bar
      baz |}]

  let%expect_test "raise exception" =
    match run non_zero with
    | () -> assert false
    | exception Subprocess_error _ -> ()

  let%expect_test "read stdout" =
    print_endline @@ read print_to_stderr;
    [%expect {|
      foo
      bar
      baz |}]

  let%expect_test "read stdout lines" =
    print_endline @@ String.concat " " @@ lines print_to_stdout;
    [%expect {| foo bar baz |}]

  let%expect_test "read stderr" =
    print_endline @@ read_err print_to_stderr;
    [%expect {|
      foo
      bar
      baz |}]

  let%expect_test "read stderr lines" =
    print_endline @@ String.concat " " @@ lines_err print_to_stderr;
    [%expect {|
      foo bar baz |}]

  let%expect_test "fold stdout" =
    fold print_to_stdout ~init:()
      ~f:(fun () line -> print_string line);
    [%expect {| foobarbaz |}]

  let%expect_test "fold stderr" =
    fold_err print_to_stderr ~init:()
      ~f:(fun () line -> print_string line);
    [%expect {| foobarbaz |}]

  let%expect_test "exec echo" =
    let output = exec (print_to_stdout |> pipe_out)
        ~f:(fun proc -> In_channel.input_all @@ stdout proc) in
    print_endline output;
    [%expect {|
      foo
      bar
      baz |}]

  let%expect_test "rescue output from error" =
    let output = ref "" in
    match exec (error_with_output |> pipe_out)
            ~f:(fun p -> output := In_channel.input_all @@ stdout p) with
    | () -> assert false
    | exception Subprocess_error _ ->
      print_endline !output;
    [%expect {| message |}]

end

module TestResultsInterface = struct
end
