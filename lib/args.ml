type t = string array

let arg_to_string s =
  let escaped = String.escaped s in
  if escaped = s then s
  else Printf.sprintf {|"%s"|} escaped

let to_string t =
  Array.to_list t
  |> List.map arg_to_string
  |> String.concat " "
