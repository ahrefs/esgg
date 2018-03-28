open ExtLib

module Json = Yojson.Safe
module U = Json.Util

let get json name conv =
  match json with
  | `Assoc a -> (try List.assoc name a |> conv with exn -> Exn.fail ~exn "get %S" name)
  | _ -> Exn.fail "get %S : not a dict" name

type mapping = { mapping : Json.json; name : string option; }

let ref_path mapping path =
  match mapping.name with
  | None -> path
  | Some base -> base :: path

module ES_name = struct

let to_ocaml s = List.map String.capitalize s |> String.concat "."
let make s = Stre.nsplitc s '.'
let show = String.concat "."

let equal (a:string list) b = a = b

end

let atd_of_es_type = function
| "long" -> `Int
| "keyword" | "text" -> `String
| "date" -> `String
| s -> Exn.fail "atd_of_es_type: cannot handle %S" s

let typeof mapping t =
  let rec find path schema =
    match path with
    | [] -> get schema "type" U.to_string
    | hd::tl -> find tl (List.assoc hd (get schema "properties" U.to_assoc))
  in
  match find t mapping.mapping with
  | exception _ -> Exn.fail "no such field"
  | "long" when List.exists (fun s -> String.exists s "hash") t -> `Int64 (* hack *)
  | a -> atd_of_es_type a

let typeof mapping x = try typeof mapping x with exn -> Exn.fail ~exn "typeof field %S" (ES_name.show x)
