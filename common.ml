open ExtLib
open Prelude

module U = struct

let member name = function
| `Assoc l -> (try List.assoc name l with _ -> `Null)
| _ -> Exn.fail "member %S : not a dict" name

let get json name conv = try member name json |> conv with exn -> Exn.fail ~exn "get %S" name

let assoc name json = match member name json with `Null -> Exn.fail "assoc %S : not found" name | x -> x

let to_string = function `String (s:string) -> s | _ -> Exn.fail "expected string"
let to_assoc = function `Assoc a -> a | _ -> Exn.fail "expected dict"

end

type mapping = { mapping : Yojson.Basic.json; name : string option; }

module ES_name = struct

type t = (string option * string list)

let to_ocaml (modname,l) = (List.filter_map id [modname] @ l) |> List.map String.capitalize |> String.concat "."
let get_path = snd
let make mapping s = mapping.name, Stre.nsplitc s '.'
let show (_,l) = String.concat "." l

let equal (a:t) b = a = b

end

type simple_type = [ `Int | `Int64 | `String | `Double ]

let show_simple_type = function
| `Int -> "int"
| `Int64 -> "int64"
| `String -> "string"
| `Double -> "float"

let simple_of_es_type = function
| "long" -> `Int
| "keyword" | "text" -> `String
| "date" -> `String
| "double" -> `Double
| s -> Exn.fail "simple_of_es_type: cannot handle %S" s

let typeof mapping t : simple_type =
  let rec find path schema =
    match path with
    | [] -> U.get schema "type" U.to_string
    | hd::tl -> find tl (List.assoc hd (U.get schema "properties" U.to_assoc))
  in
  match find (ES_name.get_path t) mapping.mapping with
  | exception _ -> Exn.fail "no such field"
  | "long" when List.exists (fun s -> String.exists s "hash") (ES_name.get_path t) -> `Int64 (* hack *)
  | a -> simple_of_es_type a

let typeof mapping x = try typeof mapping x with exn -> Exn.fail ~exn "typeof field %S" (ES_name.show x)
