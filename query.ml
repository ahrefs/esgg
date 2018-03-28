open ExtLib
open Printf

open Common

type t =
| Bool of (string * t list) list
| Term of (string * Tjson.t)
| Terms of (string * Tjson.t)
| Range of (string * Tjson.t list)
| Exists of string

let lookup json x = try Some (U.assoc x json) with _ -> None

let rec extract_clause (query,json) =
  let as_list = function `List l -> l | `Assoc _ as x -> [x] | _ -> Exn.fail "bad %S clause : expected list or dict" query in
  match query with
  | "must" | "must_not" | "should" | "filter" -> Some (query, List.map extract_query @@ as_list json)
  | _ -> None
and extract_query q =
  let q = match q with `Assoc [q] -> q | _ -> Exn.fail "bad query" in
  match q with
  | "bool", `Assoc l -> Bool (List.filter_map extract_clause l)
  | "terms", `Assoc [field, x] -> Terms (field, x) (* TODO distinguish terms lookup *)
  | "term", `Assoc [f, (`Assoc _ as x)] -> Term (f, U.assoc "value" x)
  | "term", `Assoc [f, x] -> Term (f,x)
  | "range", `Assoc [f, (`Assoc _ as x)] -> Range (f, List.filter_map (lookup x) ["gte";"gt";"lte";"lt"])
  | "exists", `Assoc ["field", `String f] -> Exists f
  | k, _ -> Exn.fail "unsupported query %S" k

let resolve_types mapping query =
  let h = Hashtbl.create 3 in
  let store var ti =
    match Hashtbl.find h var with
    | exception _ -> Hashtbl.add h var ti
    | x when ES_name.equal (fst x) (fst ti) -> ()
    | x -> Exn.fail "type mismatch for variable %S : %s <> %s" var (ES_name.to_ocaml @@ fst ti) (ES_name.to_ocaml @@ fst x)
  in
  let record var field =
    let name = ES_name.make field in
    store var (name, typeof mapping name)
  in
  let rec iter = function
  | Bool l -> List.iter (fun (_typ,l) -> List.iter iter l) l
  | Terms (field, `Var var) | Term (field, `Var var) -> record var field
  | Range (field, l) -> List.iter (function `Var var -> record var field | _ -> ()) l
  | Terms _ | Term _ | Exists _ -> ()
  in
  iter query;
  h

let extract json = extract_query @@ U.assoc "query" json

let convertor t v =
  match t with
  | `Int -> sprintf "string_of_int %s" v
  | `Int64 -> sprintf "Int64.to_string %s" v
  | `String -> sprintf "Yojson.Basic.to_string (`String %s)" v
  | `Json -> sprintf "Yojson.Basic.to_string %s" v

let analyze mapping json =
  let q = extract json in
  let h = resolve_types mapping q in
  let map name =
    match Hashtbl.find h name with
    | exception _ -> convertor `Json name
    | (host_type,atd_type) -> convertor atd_type (sprintf "(%s.unwrap %s)" (ES_name.to_ocaml host_type) name)
  in
  Tjson.lift map json;
  ()
