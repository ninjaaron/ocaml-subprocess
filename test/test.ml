let with_shell args =
  Subprocess.cmd ["sh"; "-c"; args]

let print_to_stdout = with_shell "echo 'foo\nbar\nbaz'"
let print_to_stderr = with_shell ">&2 echo 'foo\nbar\nbaz'"
let non_zero = Subprocess.cmd ["false"]
let error_with_output = with_shell "echo 'message'; false"
let long_output = with_shell {|
    for line in $(seq 1 99999); do echo "foo"; done
    |}

let out_and_err = with_shell {|
    for n in $(seq 1 100); do
      echo "out $n, part 1"
      >&2 echo "error $n"
      echo "out $n, part 2"
    done
    |}

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

  let%expect_test "multi-pipe with long output" =
    let lines =
      let& proc1 = long_output |> pipe_out in
      let& proc2 =
        cmd ["tr"; "a-z"; "A-Z"] |> channel_in (stdout proc1) |> pipe_out in
      let& proc3 = cmd ["cat"] |> channel_in (stdout proc2) |> pipe_out in
      let& proc4 =
        cmd ["tr"; "A-Z"; "a-z"] |> channel_in (stdout proc3) |> pipe_out in
      In_channel.input_lines (stdout proc4) in
    Printf.printf "%S x %n\n" (List.hd lines) (List.length lines);
    [%expect {| "foo" x 99999 |}]

  let%expect_test "test joined" =
    let n_lines = lines_joined out_and_err |> List.length in
    print_int n_lines;
    [%expect {| 300 |}]

  let%expect_test "test both" =
    let (out, err) = lines_both out_and_err in
    Printf.printf "out: %d, err: %d" (List.length out) (List.length err);
    [%expect {| out: 200, err: 100 |}]
end

module TestResultsInterface = struct
end
