open ExtLib
open Prelude

module U = struct

let member name = function
| `Assoc l -> (try List.assoc name l with _ -> `Null)
| _ -> Exn.fail "member %S : not a dict" name

let get json name conv = try member name json |> conv with exn -> Exn.fail ~exn "get %S" name
let opt json name conv = try match member name json with `Null -> None | x -> Some (conv x) with exn -> Exn.fail ~exn "opt %S" name

let assoc name json = match member name json with `Null -> Exn.fail "assoc %S : not found" name | x -> x

let to_string = function `String (s:string) -> s | _ -> Exn.fail "expected string"
let to_assoc = function `Assoc a -> a | _ -> Exn.fail "expected dict"
let to_bool = function `Bool b -> b | _ -> Exn.fail "expected bool"

end

type mapping = { mapping : Yojson.Basic.json; name : string option; }

module ES_name = struct

type t = (string option * string list)

let to_ocaml (modname,l) = (List.filter_map id [modname] @ l) |> List.map String.capitalize |> String.concat "."
let get_path = snd
let make mapping s = mapping.name, Stre.nsplitc s '.'
let append (m,l) s = m, l @ Stre.nsplitc s '.'
let show (_,l) = String.concat "." l

let equal (a:t) b = a = b

end

type simple_type = [ `Int | `Int64 | `String | `Double | `Bool ]
type nullable_type = [ simple_type | `Maybe of simple_type ]
type ref_type = [ `Ref of (ES_name.t * simple_type) ] (* reference field in mapping *)
type wire_type = [ simple_type | `Json ]
type var_type' = [ wire_type | ref_type | `List of [ ref_type | simple_type ] ]
type var_type = [ var_type' | `Optional of var_type' ]

let show_simple_type = function
| `Int -> "int"
| `Int64 -> "int64"
| `String -> "string"
| `Double -> "float"
| `Bool -> "bool"

let show_nullable_type = function
| #simple_type as t -> show_simple_type t
| `Maybe t -> show_simple_type t ^ "?"

let show_var_type' = function
| `Json -> "json"
| `Ref (_,t) | (#simple_type as t) -> show_simple_type t
| `List (`Ref (_,t) | (#simple_type as t)) -> Printf.sprintf "[%s]" (show_simple_type t)

let show_var_type = function
| #var_type' as t -> show_var_type' t
| `Optional t -> Printf.sprintf "%s?" (show_var_type' t)

let simple_of_es_type name t =
  match t with
  | "long" when List.exists (fun s -> String.exists s "hash") (ES_name.get_path name) -> `Int64 (* hack *)
  | "long" -> `Int
  | "keyword" | "text" -> `String
  | "ip" -> `String
  | "date" -> `String
  | "double" -> `Double
  | "boolean" -> `Bool
  | _ -> Exn.fail "simple_of_es_type: cannot handle %S" t

let typeof mapping t : simple_type =
  let rec find path schema =
    match path with
    | [] -> U.get schema "type" U.to_string
    | hd::tl -> find tl (List.assoc hd (U.get schema "properties" U.to_assoc))
  in
  match find (ES_name.get_path t) mapping.mapping with
  | exception _ -> Exn.fail "no such field"
  | a -> simple_of_es_type t a

let typeof mapping x = try typeof mapping x with exn -> Exn.fail ~exn "typeof field %S" (ES_name.show x)
