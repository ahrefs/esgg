open Printf
open ExtLib

open Common

type var_list = [ `List of Tjson.var list | `Var of Tjson.var ]

let var_list_of_json ~desc = function
| `List l -> `List (List.filter_map (function `Var v -> Some v | _ -> None) l)
| `Var _ as x -> x
| _ -> fail "bad %s : expecting list or variable" desc

type query_t =
| Bool of (string * query list) list
| Field of { field : string; cardinality : cardinality; values : Tjson.t list }
| Var of Tjson.var
| Strings of var_list
| Nothing
and query = { json : Tjson.t; query : query_t; cstrs : constraint_t list; }

type t =
| Search of { q : query; extra : constraint_t list; source : source_filter or_var option; fields : string list option; highlight : string list option; }
| Mget of { ids: var_list; json: Tjson.t; conf: Tjson.t } (* get and mget probably can/should share most of the type? *)
| Get of { id : Tjson.var; return : [ `Source of source_filter | `Fields of string list | `Nothing ] }

module Variable = struct

 type t = Property of cardinality * ES_name.t * simple_type | Any | Type of simple_type | List of simple_type

 let show = function
 | Any -> "any"
 | Type typ -> show_simple_type typ
 | List typ -> sprintf "[%s]" (show_simple_type typ)
 | Property (cardinality,name,typ) ->
  let s = sprintf "%s:%s" (ES_name.show name) (show_simple_type typ) in
  match cardinality with
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

let cardinality_of_qt = function "terms" -> Many | _ -> One

let rec extract_clause (clause,json) =
  try match clause with
  | "must" | "must_not" | "should" | "filter" ->
    begin match json with
    | `Var v -> (json, [], (clause, [ {json; query=Var v; cstrs = []} ]))
    | `Assoc _ as x -> let q = extract_query x in (q.json, [], (clause, [q]))
    | `List l ->
      let l = List.map extract_query l in
      let json = `List (List.map (fun q -> q.json) l) in
      (json, [], (clause, l))
    | _ -> fail "bad %S clause : expected list or dict" clause
    end
  | "minimum_should_match" ->
    begin match json with
    | `Int _ | `String _ -> json, [], (clause, [])
    | `Var v -> json, [On_var (v,(Eq_type Int))], (clause, [])
    | _ -> fail "bad %S clause : expected int or string" clause
    end
  | _ -> fail "unsupported clause"
  with
    exn -> fail ~exn "clause %S" clause
and extract_query json =
  let (json,cstrs,query) =
    match json with
    | `Var v -> json, [], Var v
    | `Assoc [qt,qv] ->
      begin match qt, qv with
      | ("function_score"|"nested"), `Assoc l ->
        begin match List.assoc "query" l with
        | exception _ when qt = "nested" -> fail "nested query requires query, duh"
        | exception _ when qt = "function_score" -> json, [], Nothing
        | q ->
          let { json; query; cstrs } = extract_query q in
          `Assoc [qt, Tjson.replace qv "query" json], cstrs, query
        end
      | "bool", `Assoc l ->
        let bool = List.map extract_clause l in
        let json = `Assoc ["bool", `Assoc (List.map (fun (json,_cstr,(clause,_)) -> clause, json) bool)] in
        json, List.concat @@ List.map (fun (_,c,_) -> c) bool, Bool (List.map (fun (_json,_cstr,x) -> x) bool)
      | "ids", x -> json, [], Strings (var_list_of_json ~desc:"ids values" (U.assoc "values" x))
      | "query_string", x -> json, [], (match U.assoc "query" x with `Var x -> Strings (`List [x]) | _ -> Nothing)
      | ("match_all"|"match_none"), _ -> json, [], Nothing
      | _qt, `Var x -> json, [], Var x
      | "range", `Assoc [_f, `Var x] -> json, [], Var x
      | _ ->
        let field, values =
          (* For simple single-field queries, store relation of one field to one or more values (with or without variables) *)
          try
            match qt with
            (* term level queries *)
            | "term"
            | "prefix"
            | "wildcard"
            | "regexp"
            | "fuzzy"
            | "type" ->
              begin match qv with
              | `Assoc [f, (`Assoc _ as x)] -> f, [U.assoc "value" x]
              | `Assoc [f, x] -> f, [x]
              | _ -> fail "unrecognized term level query payload"
              end
            (* full text queries *)
            | "common"
            | "match"
            | "match_phrase"
            | "match_bool_prefix"
            | "match_phrase_prefix" ->
              begin match qv with
              | `Assoc [f, (`Assoc _ as x)] -> f, [U.assoc "query" x]
              | `Assoc [f, x] -> f, [x]
              | _ -> fail "unrecognized full text query payload"
              end
            | _ ->
            match qt, qv with
            | "exists", `Assoc ["field", `String f] -> f, []
            | "terms", `Assoc [f, x] -> f, [x] (* TODO distinguish terms lookup *)
            | "range", `Assoc [f, (`Assoc _ as x)] -> f, List.filter_map (lookup x) ["gte";"gt";"lte";"lt"]
            | _ -> fail "unrecognized query"
          with exn ->
            fail ~exn "dsl query %S" qt
        in
        json, [], Field { field; cardinality = cardinality_of_qt qt; values }
      end
    | _ -> fail "bad query"
  in
  let json =
    match List.filter_map (fun {Tjson.optional;list=_;name} -> if optional then Some name else None) @@ Tjson.get_vars ~optional:false json with
    | [] -> json
    | vars ->
      if Tjson.debug_dump then printfn "introducing optional scope for : %s" (String.concat " " vars);
      let label =
        match vars with
        | [var] -> var (* single variable group should be named same as variable *)
        | _ ->
        match query with
        | Field { field; _ } -> field
        | _ -> String.concat "_" vars
      in
      let label = to_valid_ident ~prefix:"f_" label in (* XXX *)
      `Optional ({ label; vars }, json)
  in
  { query; json; cstrs }

let record vars var ti =
  match Hashtbl.find vars var with
  | exception _ -> Hashtbl.add vars var ti
  | x when Variable.equal x ti -> ()
  | x -> fail "type mismatch for variable %S : %s <> %s" var (Variable.show ti) (Variable.show x)

let infer' c0 query =
  let constraints = ref c0 in
  let rec iter { query; json=_; cstrs } =
    cstrs |> List.iter (tuck constraints);
    match query with
    | Bool l -> List.iter (fun (_typ,l) -> List.iter iter l) l
    | Field { field; cardinality; values } ->
      List.iter (function `Var var -> tuck constraints (On_var (var, Eq_field (cardinality,field))) | _ -> ()) values
    | Var var -> tuck constraints (On_var (var, Eq_any))
    | Strings (`Var var) -> tuck constraints (On_var (var, Eq_list String))
    | Strings (`List l) -> l |> List.iter (function var -> tuck constraints (On_var (var, Eq_type String)))
    | Nothing -> ()
  in
  iter query;
  !constraints

let infer = infer' []

let record_single_var typ (v:Tjson.var) =
  let vars = Hashtbl.create 3 in
  record vars v.name typ;
  vars

let resolve_mget_types ids =
  match ids with
  | `Var (v:Tjson.var) -> record_single_var (List String) v
  | _ -> fail "mget: only variable ids supported"

let resolve_get_types = record_single_var (Type String)

let get_var json name =
  match U.member name json with
  | `Var v -> assert (v.Tjson.optional = false); Some v
  | _ -> None

let extract_conf json =
  match U.member "_esgg" json with
  | `Null -> `Assoc []
  | j -> j

let extract_source json =
  let source = U.member "_source" json in
  match U.member "size" json = `Int 0 || source = `Bool false with
  | true -> None
  | false ->
    let filter =
      match source with
      | `Var v -> Dynamic v
      | _ ->
        let (excludes,includes) =
          match source with
          | `List l -> None, Some (List.map U.to_string l)
          | `String s -> None, Some [s]
          | _ -> source_fields "excludes" json, source_fields "includes" json
        in
        Static { excludes; includes; }
    in
    Some filter

let extract_source_static json =
  match extract_source json with
  | None -> None
  | Some (Static f) -> Some f
  | Some (Dynamic _) -> fail "dynamic source not supported here" (* because source_args *)

let extract_highlight json =
  match U.opt "highlight" id json with
  | None -> None
  | Some json -> Some (U.get "fields" U.to_assoc json |> List.map fst)

let extract_stored_fields json =
  match U.opt "stored_fields" id json with
  | None -> None
  | Some (`List l) -> Some (List.map U.to_string l)
  | _ -> fail "stored_fields expected to be a list of strings"

(** Filter out the [_esgg] field from the json if it exist. *)
let filter_out_conf json =
  match json with
  | `Assoc l -> `Assoc (List.filter (fun (k, _v) -> k <> "_esgg") l)
  | j -> j

let extract json =
  match U.assoc "query" json with
  | q ->
    let extra = List.map (fun v -> On_var (v, Eq_type Int)) @@ List.filter_map (get_var json) ["size";"from"] in
    Search { q = extract_query q; extra; source = extract_source json; fields = extract_stored_fields json; highlight = extract_highlight json; }
  | exception _ ->
    match U.assoc "id" json with
    | `Var id ->
      let return =
        match extract_source_static json, extract_stored_fields json with
        | Some _, Some _ -> fail "get request allows either source or stored_fields, but not both"
        | Some source, None -> `Source source
        | None, Some fields -> `Fields fields
        | None, None -> `Nothing
      in
      Get { id; return; }
    | _ -> fail "only variable id supported for get request"
    | exception _ ->
      let conf = extract_conf json in
      let json = filter_out_conf json in
      let ids =
        match U.assoc "docs" json with
        | `List l -> var_list_of_json ~desc:"mget docs" (`List (List.map U.(assoc "_id") l))
        | _ -> fail "unexpected docs"
        | exception _ ->
          match U.(assoc "ids" json) with
          | ids -> var_list_of_json ~desc:"mget ids" ids
          | exception _ -> fail "unrecognized ES toplevel query type, expected one of : id, ids, docs, query"
      in
      Mget { ids; json; conf }

let resolve_constraints mapping l =
  let typeof field =
    let name = ES_name.make mapping field in
    let typ = typeof mapping name in
    name, typ
  in
  let vars = Hashtbl.create 3 in
  l |> List.iter begin function
  | On_var (var,t) ->
    let t = match t with
    | Eq_type typ -> Variable.Type typ
    | Eq_list typ -> List typ
    | Eq_object -> Any (* TODO object of json *)
    | Eq_any -> Any
    | Eq_field (cardinality,field) ->
      let (name,typ) = typeof field in
      Property (cardinality,name,typ)
    in
    record vars var.name t
  | Field_num (Field f) ->
    begin match snd @@ typeof f with
    | Int | Int64 | Double -> ()
    | String | Date (* FIXME it is actually numeric wrt constraints ? *)
    | Bool | Json as t -> eprintfn "W: field %S expected to be numeric, but has type %s" f (show_simple_type t)
    end
  | Field_num (Script _) -> ()
  | Field_date _ -> ()
  end;
  vars
