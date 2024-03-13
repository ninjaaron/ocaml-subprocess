module Type = struct
  type t =
    | Abstract of {t: string}
    | Custom of {show: (t -> string); value: t}
    | Record of (string * t) list
    | Variant of string * t
    | EmptyVariant of string
    | Tuple of t list
    | List of t list
    | Array of t array
    | Int of int
    | Float of float
    | String of string
    | Char of char
    | Bool of bool
    | Unit
end

include Type

let rec show t =
  let open Printf in
  match t with
  | Abstract {t} -> sprintf "<abstract %s>" t
  | Custom c -> c.show c.value
  | Record fields ->
    sprintf "{%s}" (String.concat "; " (List.map show_field fields))
  | EmptyVariant tag -> tag
  | Variant (tag, value) ->
    sprintf "%s %s" tag (show value)
  | Tuple fields -> 
    sprintf "(%s)" (String.concat ", " (List.map show fields))
  | List l -> sprintf "[%s]" (String.concat "; " (List.map show l))
  | Array a ->
    sprintf "[|%s|]" (String.concat "; " (List.map show (Array.to_list a)))
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | String s -> sprintf {|"%s"|} (String.escaped s)
  | Char c -> sprintf "%c" c
  | Bool b -> string_of_bool b
  | Unit -> "()"
and show_field (name, value) =
  Printf.sprintf "%s=%s" name (show value)

let pp_sep c out () =
  Format.fprintf out "%c@ " c

let pp_list ~f ~out r sep l t =
  Format.(fprintf out "%s@[<hov>%a@]%s" r
            (pp_print_list ~pp_sep:(pp_sep sep) f) t l)

let rec pp ?f out t =
  let open Format in 
  match f, t with
  | Some f, Custom {value; _}-> f out value
  | None, _ | Some _, _->
    match t with
    | Abstract _ | Custom _ | EmptyVariant _ | Int _ | Float _
    | String _ | Char _ | Bool _ | Unit -> fprintf out "%s" (show t)
    | Record l -> pp_list ~out ~f:pp_field "{" ';' "}" l
    | Variant (tag, t) -> fprintf out "%s %a" tag (pp ?f) t
    | Tuple l -> pp_list ~out ~f:(pp ?f) "(" ',' ")" l
    | List l -> pp_list ~out ~f:(pp ?f) "[" ';' "]" l
    | Array a ->
      Format.(
        fprintf out "[|@[%a@]|]"
          (pp_print_array ~pp_sep:(pp_sep ';') (pp ?f)) a
      )
and pp_field ?f out (label, t) =
  Format.fprintf out "%s=%a" label (pp ?f) t

let pair (a, b) fa fb = Tuple [fa a; fb b]
let list ~f l = List (List.map f l)
let array ~f a = Array (Array.map f a)
let option ~f = function
  | Some value -> Variant ("Some", f value)
  | None -> EmptyVariant "None"
let result ~ok ~error = function
  | Ok value -> Variant ("Ok", ok value)
  | Error value -> Variant ("Error", error value)
let int i = Int i
let float f = Float f
let string s = String s
let char c = Char c
let bool b = Bool b 
let unit () = Unit

module To = struct
  include Type
  let pair t fa fb =
    match t with
    | Tuple [a; b] ->
      fa a, fb b
    | _ -> failwith "not a pair"
  let list ~f = function
    | List l -> List.map f l
    | _ -> failwith "not a list"
  let array ~f = function
    | Array a -> Array.map f a
    | _ -> failwith "not an array"
  let option ~f = function
    | EmptyVariant "None" -> None
    | Variant ("Some", v) -> Some (f v)
    | _ -> failwith "not a option type"
  let result ~ok ~error = function
    | Variant ("Ok", v) -> Ok (ok v)
    | Variant ("Error", v) -> Error (error v)
    | _ -> failwith "not a result"
  let int = function
    | Int i -> i
    | _ -> failwith "not an int"
  let float = function
    | Float f -> f
    | _ -> failwith "not a float"
  let string = function
    | String s -> s
    | _ -> failwith "not a string"
  let char = function
    | Char c -> c
    | _ -> failwith "not a char"
  let bool = function
    | Bool b -> b
    | _ -> failwith "not a bool"
end
