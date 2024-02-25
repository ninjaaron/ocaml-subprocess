type t = List of t list | Atom of string

let rec show = function
  | Atom s ->
    let escaped = String.escaped s in
    if s <> escaped || String.contains s ' '
    then Printf.sprintf {|"%s"|} escaped
    else s
  | List l ->
    let format () l' = String.concat " " (List.map show l') in
    Printf.sprintf "(%a)" format l

let rec pp ppf = function
  | Atom _ as t ->
    let s = show t in
    Format.fprintf ppf "%s" s
  | List l ->
    let pp_sep ppf () =  Format.fprintf ppf "@ " in
    Format.fprintf ppf "(@[<hov>%a@])" (Format.pp_print_list ~pp_sep pp) l

module Parse = struct
  type token =
    | L_par
    | R_par
    | String of string
    | End

  let dump buf =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s

  let rec collect_string idx buf s =
    let open Buffer in
    match s.[idx] with
    | '\\' ->
      add_char buf '\\';
      add_char buf s.[idx+1];
      collect_string (idx+2) buf s
    | '"' -> idx + 1, String (Scanf.unescaped (dump buf))
    | c ->
      add_char buf c;
      collect_string (idx+1) buf s

  let rec collect_symbol idx buf s =
    let open Buffer in
    if String.length s = idx then
      idx, String (dump buf)
    else
      match s.[idx] with
      | ' ' | '\n' | '\t' -> idx + 1, String (dump buf)
      | '(' | ')' -> idx, String (dump buf)
      | c ->
        add_char buf c;
        collect_symbol (idx+1) buf s

  let rec lex ~idx ~buf s =
    if idx >= String.length s then idx, End
    else (
      match s.[idx] with
      | ' ' | '\n' | '\t' -> lex ~idx:(idx+1) ~buf s
      | '(' -> idx + 1, L_par
      | ')' -> idx + 1, R_par
      | '"' -> collect_string (idx+1) buf s
      | _ -> collect_symbol idx buf s
    )

  let rec get_list ~idx ~buf s =
    let idx, tok = lex ~idx ~buf s in
    match tok with
    | L_par ->
      let idx, hd = get_list ~idx ~buf s in
      let idx, tl = get_list ~idx ~buf s in
      idx, List hd :: tl
    | R_par -> idx, []
    | String str ->
      let idx, tl = get_list ~idx ~buf s in
      idx, Atom str :: tl
    | _ -> failwith "couldn't parse sexp."

  let parse s =
    let buf = Buffer.create 1024 in
    let idx, tok = lex ~idx:0 ~buf s in
    match tok with
    | String str -> Atom str
    | L_par ->
      let _, l = get_list ~idx ~buf s in
      List l
    | _ -> failwith "couldn't parse sexp."

end 

let atom_exn = function
  | Atom s -> s
  | _ -> failwith "not an atom"

let list_exn = function
  | List l -> l
  | _ -> failwith "not a list"

let mkatom f a = Atom (f a)
let unwrap_atom f a = f (atom_exn a)


module Of = struct
  let list ~f lst = List (List.map f lst)
  let array ~f arr = list ~f (Array.to_list arr)
  let field (name, value) f = List [Atom name; f value]
  let int = mkatom Int.to_string
  let float = mkatom Float.to_string
  let bool = mkatom string_of_bool
  let char c =
    let buf = Buffer.create 1 in
    Buffer.add_char buf c;
    Atom (Buffer.contents buf)
  let unit () = List []
  let string s = Atom s
  let parse = Parse.parse
end

module To = struct
  let list ~f l = List.map f (list_exn l)
  let array ~f l = Array.of_list (list ~f l)
  let field name l f =
    match l with
    | List [Atom name'; value] when name' = name ->
      f value
    | _ -> failwith "field does not match"
  let int = unwrap_atom int_of_string
  let float = unwrap_atom Float.of_string
  let bool = unwrap_atom bool_of_string
  let char = unwrap_atom (fun s -> String.get s 0)
  let unit = function
    | List [] -> ()
    | _ -> failwith "unit can only be realized from an empty list"
  let string = atom_exn
end






