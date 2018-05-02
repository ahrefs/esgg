open Printf
open ExtLib
open Prelude

open Common

(* type query_type = Term | Terms | Range | Exists *)
type query_type = string
type equation = Field of { field : string; values : Tjson.t list } | Var of Tjson.var

type query =
| Bool of (string * t list) list
| Query of query_type * equation
and t = { json : Tjson.t; query : query }

type req = Search of t | Mget of Tjson.t

module Variable = struct

 type multi = One | Many
 type t = Property of multi * ES_name.t * simple_type | Any | Type of simple_type | List of simple_type

 let show = function
 | Any -> "any"
 | Type typ -> show_simple_type typ
 | List typ -> sprintf "[%s]" (show_simple_type typ)
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

let rec extract_clause (clause,json) =
  match clause with
  | "must" | "must_not" | "should" | "filter" ->
    begin match json with
    | `Assoc _ as x -> let q = extract_query x in q.json, (clause, [q])
    | `List l ->
      let l = List.map extract_query l in
      let json = `List (List.map (fun q -> q.json) l) in
      json, (clause, l)
    | _ -> Exn.fail "bad %S clause : expected list or dict" clause
    end
  | _ -> Exn.fail "unsupported bool clause %S" clause
and extract_query json =
  let q = match json with `Assoc [q] -> q | _ -> Exn.fail "bad query" in
  let (json,query) = match q with
  | "bool", `Assoc l ->
    let bool = List.map extract_clause l in
    let json = `Assoc ["bool", `Assoc (List.map (fun (json,(clause,_)) -> clause, json) bool)] in
    json, Bool (List.map snd bool)
  | qt, `Var x -> json, Query (qt, Var x)
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
    json, Query (qt, Field { field; values })
  in
  let json =
    match List.filter_map (fun { Tjson.optional; name } -> if optional then Some name else None) @@ Tjson.vars ~optional:false json with
    | [] -> json
    | [var_name] -> `Optional (var_name, json)
    | _ -> Exn.fail "multiple optional vars in one scope not supported"
  in
  { query; json }

let record vars var ti =
  match Hashtbl.find vars var with
  | exception _ -> Hashtbl.add vars var ti
  | x when Variable.equal x ti -> ()
  | x -> Exn.fail "type mismatch for variable %S : %s <> %s" var (Variable.show ti) (Variable.show x)

let resolve_types mapping query =
  let vars = Hashtbl.create 3 in
  let rec iter { query; json=_ } =
    match query with
    | Bool l -> List.iter (fun (_typ,l) -> List.iter iter l) l
    | Query (qt,Field { field; values }) ->
      let name = ES_name.make mapping field in
      let typ = typeof mapping name in
      let multi = match qt with "terms" -> Variable.Many | _ -> One in
      List.iter (function `Var (var:Tjson.var) -> record vars var.name (Property (multi,name,typ))  | _ -> ()) values
    | Query (_,Var var) -> record vars var.name Any
  in
  iter query;
  vars

let resolve_mget_types ids =
  let vars = Hashtbl.create 3 in
  match ids with
  | `Var (v:Tjson.var) -> record vars v.name (List `String); vars
  | _ -> Exn.fail "mget: only variable ids supported"

let extract json =
  match U.assoc "query" json with
  | q -> Search (extract_query q)
  | exception _ ->
    let ids = U.assoc "ids" json in
    Mget ids

let convert_wire_type = function
| `Int -> sprintf "string_of_int %s"
| `Int64 -> sprintf "Int64.to_string %s"
| `String -> sprintf "Json.to_string (`String %s)"
| `Double -> sprintf "Json.to_string (`Double %s)"
| `Bool -> sprintf "string_of_bool %s"
| `Json -> sprintf "Json.to_string %s"

let convertor (t:var_type') unwrap name =
  match t with
  | `Ref (_,t) -> convert_wire_type t (unwrap name)
  | #wire_type as t -> convert_wire_type t (unwrap name)
  | `List (`Ref (_,t) | (#simple_type as t)) ->
    let mapper =
      sprintf @@ match t with
      | `Int -> "`Int %s"
      | `Int64 -> "`String (Int64.to_string %s)"
      | `String -> "`String %s"
      | `Double -> "`Double %s"
      | `Bool -> "`Bool %s"
    in
    sprintf "Json.to_string (`List (List.map (fun x -> %s) %s))" (mapper @@ unwrap "x") name

let resolve_constraints vars l =
  l |> List.iter begin function
  | `Var (typ, (var:Tjson.var)) -> record vars var.name (Type typ)
  | `Is_num _ | `Is_date _ -> ()
  end

let analyze mapping json =
  let (vars,json) =
    match extract json with
    | Search q ->
      let constraints = List.concat @@ fst @@ List.split @@ Derive.analyze_aggregations json in
      let vars = resolve_types mapping q in
      resolve_constraints vars constraints;
      let json =
        match json with
        | `Assoc l -> `Assoc (List.map (function "query",_ -> "query", q.json | x -> x) l)
        | _ -> assert false
      in
      vars, json
    | Mget ids -> resolve_mget_types ids, json
  in
  let var_unwrap name =
    match Hashtbl.find vars name with
    | exception _ -> id
    | Property (_,es_name,_) -> sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name)
    | Any | Type _ | List _ -> id
  in
  let var_type name : var_type' =
    match Hashtbl.find vars name with
    | Property (One,name,typ) -> `Ref (name,typ)
    | Property (Many,name,typ) -> `List (`Ref (name,typ))
    | exception _ -> `Json
    | Any -> `Json
    | List typ -> (`List typ :> var_type')
    | Type typ -> (typ:>var_type')
  in
  let map name = convertor (var_type name) (var_unwrap name) name in
  let vars = Tjson.vars ~optional:true json |> List.map begin fun (var:Tjson.var) ->
    let typ = var_type var.name in
    var.name, if var.optional then `Optional typ else (typ:>var_type) end
  in
  vars, map, json
