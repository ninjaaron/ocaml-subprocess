type t = List of t list | Atom of string

let rec show buf = function
  | List l ->
    Buffer.add_char buf '(';
    List.iter (show buf) l;
    Buffer.truncate buf (Buffer.length buf - 1);
    Buffer.add_string buf ") "
  | Atom s ->
    let escaped = String.escaped s in
    if s <> escaped || String.contains s ' '
    then (
      Buffer.add_char buf '"';
      Buffer.add_string buf escaped;
      Buffer.add_string buf "\" "
    ) else (
      Buffer.add_string buf s;
      Buffer.add_char buf ' '
    )

let show t =
  let buf = Buffer.create 1024 in
  show buf t;
  Buffer.truncate buf (Buffer.length buf - 1);
  Buffer.contents buf

let rec pp ppf = function
  | Atom _ as t ->
    let s = show t in
    Format.fprintf ppf "%s" s
  | List l ->
    let pp_sep ppf () =  Format.fprintf ppf "@ " in
    Format.fprintf ppf "(@[<hov>%a@])" (Format.pp_print_list ~pp_sep pp) l
[@@ocaml.toplevel_printer]

let l lst = List lst
let a s = Atom s
let of_strings l = List (List.map (fun s -> Atom s) l)
