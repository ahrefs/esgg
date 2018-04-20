open Printf
open ExtLib
open Prelude

open Common

(* type query_type = Term | Terms | Range | Exists *)
type query_type = string
type equation = Field of { field : string; values : Tjson.t list } | Var of string

type query =
| Bool of (string * t list) list
| Query of query_type * equation
and t = { json : Tjson.t; query : query }

module Variable = struct

 type multi = One | Many
 type t = Property of multi * ES_name.t * simple_type | Any | Type of simple_type

 let show = function
 | Any -> "any"
 | Type typ -> show_simple_type typ
 | Property (multi,name,typ) ->
  let s = sprintf "%s:%s" (ES_name.show name) (show_simple_type typ) in
  match multi with
  | One -> s
  | Many -> sprintf "[%s]" s

 let equal a b =
    match a,b with
    | Any, Any -> true
    | Type a, Type b -> a = b
    | Property (m1,n1,t1), Property (m2,n2,t2) -> ES_name.equal n1 n2 && t1 = t2 && m1 = m2
    | _ -> false

end

let lookup json x = try Some (U.assoc x json) with _ -> None

let rec extract_clause (query,json) =
  let as_list = function `List l -> l | `Assoc _ as x -> [x] | _ -> Exn.fail "bad %S clause : expected list or dict" query in
  match query with
  | "must" | "must_not" | "should" | "filter" -> Some (query, List.map extract_query @@ as_list json)
  | _ -> None
and extract_query json =
  let q = match json with `Assoc [q] -> q | _ -> Exn.fail "bad query" in
  let query = match q with
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
  in
  { query; json }

let record vars var ti =
  match Hashtbl.find vars var with
  | exception _ -> Hashtbl.add vars var ti
  | x when Variable.equal x ti -> ()
  | x -> Exn.fail "type mismatch for variable %S : %s <> %s" var (Variable.show ti) (Variable.show x)

let resolve_types mapping query =
  let vars = Hashtbl.create 3 in
  let rec iter = function
  | Bool l -> List.iter (fun (_typ,l) -> List.iter (fun { query; json=_ } -> iter query) l) l
  | Query (qt,Field { field; values }) ->
    let name = ES_name.make mapping field in
    let typ = typeof mapping name in
    let multi = match qt with "terms" -> Variable.Many | _ -> One in
    List.iter (function `Var var -> record vars var (Property (multi,name,typ))  | _ -> ()) values
  | Query (_,Var var) -> record vars var Any
  in
  iter query;
  vars

let extract json = extract_query @@ U.assoc "query" json

let convert_wire_type = function
| `Int -> sprintf "string_of_int %s"
| `Int64 -> sprintf "Int64.to_string %s"
| `String -> sprintf "Json.to_string (`String %s)"
| `Double -> sprintf "Json.to_string (`Double %s)"
| `Json -> sprintf "Json.to_string %s"

let convertor (t:var_type) unwrap name =
  match t with
  | `Ref (_,t) -> convert_wire_type t (unwrap name)
  | #wire_type as t -> convert_wire_type t (unwrap name)
  | `List (`Ref (_,t)) ->
    let mapper =
      sprintf @@ match t with
      | `Int -> "`Int %s"
      | `Int64 -> "`String (Int64.to_string %s)"
      | `String -> "`String %s"
      | `Double -> "`Double %s"
    in
    sprintf "Json.to_string (`List (List.map (fun x -> %s) %s))" (mapper @@ unwrap "x") name

let resolve_constraints vars l =
  l |> List.iter begin function
  | `Var (typ, var) -> record vars var (Type typ)
  | `Is_num _ | `Is_date _ -> ()
  end

let analyze mapping json =
  let constraints = List.concat @@ fst @@ List.split @@ Derive.analyze_aggregations json in
  let q = extract json in
  let vars = resolve_types mapping q.query in
  resolve_constraints vars constraints;
  let var_unwrap name =
    match Hashtbl.find vars name with
    | exception _ -> id
    | Property (_,es_name,_) -> sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name)
    | Any | Type _ -> id
  in
  let var_type name : var_type =
    match Hashtbl.find vars name with
    | Property (One,name,typ) -> `Ref (name,typ)
    | Property (Many,name,typ) -> `List (`Ref (name,typ))
    | exception _ -> `Json
    | Any -> `Json
    | Type typ -> (typ:>var_type)
  in
  let map name = convertor (var_type name) (var_unwrap name) name in
  let vars = Tjson.vars json |> List.map begin fun name -> name, var_type name end in
  let json =
    match json with
    | `Assoc l -> `Assoc (List.map (function "query",_ -> "query", q.json | x -> x) l)
    | _ -> assert false
  in
  vars, map, json
