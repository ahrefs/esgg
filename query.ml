open ExtLib
open Printf

open Common

(* type query_type = Term | Terms | Range | Exists *)
type query_type = string
type equation = Field of { field : string; values : Tjson.t list } | Var of string

type t =
| Bool of (string * t list) list
| Query of query_type * equation

module Variable = struct

 type t = Property of ES_name.t * simple_type | Any

 let show = function
 | Any -> "any"
 | Property (name,typ) -> sprintf "%s:%s" (ES_name.show name) (simple_type_show typ)

 let equal a b =
    match a,b with
    | Any, Any -> true
    | Property (a,a'), Property (b,b') -> ES_name.equal a b && a' = b'
    | Property _, Any | Any, Property _ -> false

end

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
  | qt, `Var x -> Query (qt, Var x)
  | qt, v ->
    let field, values =
      match qt, v with
      | "exists", `Assoc ["field", `String f] -> f, []
      | "terms", `Assoc [f, x] -> f, [x] (* TODO distinguish terms lookup *)
      | "term", `Assoc [f, (`Assoc _ as x)] -> f, [U.assoc "value" x]
      | "term", `Assoc [f, x] -> f, [x]
      | "range", `Assoc [f, (`Assoc _ as x)] -> f, List.filter_map (lookup x) ["gte";"gt";"lte";"lt"]
      | k, _ -> Exn.fail "unsupported query %S" k
    in
    Query (qt, Field { field; values })

let resolve_types mapping query =
  let h = Hashtbl.create 3 in
  let record var ti =
    match Hashtbl.find h var with
    | exception _ -> Hashtbl.add h var ti
    | x when Variable.equal x ti -> ()
    | x -> Exn.fail "type mismatch for variable %S : %s <> %s" var (Variable.show ti) (Variable.show x)
  in
  let record_field var name = record var (Property (name, typeof mapping name)) in
  let rec iter = function
  | Bool l -> List.iter (fun (_typ,l) -> List.iter iter l) l
  | Query (_,Field { field; values }) -> List.iter (function `Var var -> record_field var (ES_name.make field) | _ -> ()) values
  | Query (_,Var var) -> record var Any
  in
  iter query;
  h

let extract json = extract_query @@ U.assoc "query" json

let convertor t =
  match t with
  | `Int -> sprintf "string_of_int %s"
  | `Int64 -> sprintf "Int64.to_string %s"
  | `String -> sprintf "Yojson.Basic.to_string (`String %s)"
  | `Double -> sprintf "Yojson.Basic.to_string (`Double %s)"
  | `Json -> sprintf "Yojson.Basic.to_string %s"

let analyze mapping json =
  let q = extract json in
  let h = resolve_types mapping q in
  let map name =
    match Hashtbl.find h name with
    | exception _ -> convertor `Json name
    | Property (es_name,typ) -> convertor typ (sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name) name)
    | Any -> convertor `Json name
  in
  Tjson.lift map json;
  ()
