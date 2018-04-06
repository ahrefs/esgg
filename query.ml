open Printf
open ExtLib
open Prelude

open Common

(* type query_type = Term | Terms | Range | Exists *)
type query_type = string
type equation = Field of { field : string; values : Tjson.t list } | Var of string

type t =
| Bool of (string * t list) list
| Query of query_type * equation

module Variable = struct

 type t = Property of ES_name.t * simple_type | Any | Type of simple_type

 let show = function
 | Any -> "any"
 | Type typ -> show_simple_type typ
 | Property (name,typ) -> sprintf "%s:%s" (ES_name.show name) (show_simple_type typ)

 let equal a b =
    match a,b with
    | Any, Any -> true
    | Type a, Type b -> a = b
    | Property (a,a'), Property (b,b') -> ES_name.equal a b && a' = b'
    | _ -> false

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

let record vars var ti =
  match Hashtbl.find vars var with
  | exception _ -> Hashtbl.add vars var ti
  | x when Variable.equal x ti -> ()
  | x -> Exn.fail "type mismatch for variable %S : %s <> %s" var (Variable.show ti) (Variable.show x)

let resolve_types mapping query =
  let vars = Hashtbl.create 3 in
  let record_field var name = record vars var (Property (name, typeof mapping name)) in
  let rec iter = function
  | Bool l -> List.iter (fun (_typ,l) -> List.iter iter l) l
  | Query (_,Field { field; values }) -> List.iter (function `Var var -> record_field var (ES_name.make field) | _ -> ()) values
  | Query (_,Var var) -> record vars var Any
  in
  iter query;
  vars

let extract json = extract_query @@ U.assoc "query" json

let convertor t =
  match t with
  | `Int -> sprintf "string_of_int %s"
  | `Int64 -> sprintf "Int64.to_string %s"
  | `String -> sprintf "Json.to_string (`String %s)"
  | `Double -> sprintf "Json.to_string (`Double %s)"
  | `Json -> sprintf "Json.to_string %s"

type var_type = [ simple_type | `Json ]

let resolve_constraints vars l =
  l |> List.iter begin function
  | `Var (typ, var) -> record vars var (Type typ)
  | `Is_num _ | `Is_date _ -> ()
  end

let analyze_ map mapping json =
  let constraints = List.concat @@ fst @@ List.split @@ Derive.analyze_aggregations json in
  let q = extract json in
  let vars = resolve_types mapping q in
  resolve_constraints vars constraints;
  let var_unwrap name =
    match Hashtbl.find vars name with
    | exception _ -> map name
    | Property (es_name,_) -> sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name) (map name)
    | Any | Type _ -> map name
  in
  let var_type name =
    match Hashtbl.find vars name with
    | Property (_,typ) -> (typ:>var_type)
    | exception _ -> `Json
    | Any -> `Json
    | Type typ -> (typ:>var_type)
  in
  let map name = convertor (var_type name) (var_unwrap name) in
  let vars = Tjson.vars json |> List.map begin fun name -> name, var_type name end in
  vars, map

let analyze = analyze_ id
