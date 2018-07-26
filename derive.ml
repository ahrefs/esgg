open ExtLib
open Prelude
open Printf

open Common

let resolve_types mapping shape =
  let rec map : resolve_type -> result_type = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Typeof x -> let name = ES_name.make mapping x in `Ref (name, typeof mapping name)
  | `Maybe t -> `Maybe (map t)
  | `Int64 | `Int | `String | `Double | `Bool | `Ref _ as t -> t
  in
  map shape

let output mapping query =
  let shape =
    match Query.extract query with
    | Get (_,filter) -> Hit.doc @@ `Maybe (Hit.of_mapping ~filter mapping)
    | Mget _ -> `Dict ["docs",`List (Hit.doc (Hit.of_mapping mapping))] (* TODO `Maybe *)
    | Search { filter; _ } ->
      let source =
        if Hit.source_requested query then
          None
        else
          Some (Hit.of_mapping ~filter mapping)
      in
      let aggs = List.map snd @@ Aggregations.analyze query in (* XXX discarding constraints *)
      let hits = List.concat [
        ["total", `Int];
        (match source with None -> [] | Some source -> ["hits", `List (Hit.doc (source:>resolve_type))]);
      ]
      in
      let result = `Dict (("hits", `Dict hits) :: (if aggs = [] then [] else ["aggregations", `Dict aggs])) in
      resolve_types mapping result
  in
  Atdgen.of_shape "result" shape

let print_reflect name mapping =
  let extern name = name ^ "_" in
  let rec iter hash nr_indent (name,x) =
    let indent = String.make nr_indent ' ' in
    let hash = hash || String.exists name "hash" in
    match U.member "type" x, U.member "properties" x with
    | `String typ, `Null ->
      let typ = if typ = "long" && hash then "int64" else typ in
      printfn "%smodule %s = %s(%s)" indent (to_valid_modname name) (extern "Id") (to_valid_modname @@ extern typ)
    | (`Null | `String "nested"), `Assoc props ->
      let modul = to_valid_modname name in
      printfn "%smodule %s = struct" indent modul;
      List.iter (iter hash (nr_indent+2)) props;
      printfn "%send (* %s *)" indent modul
    | `Null, `Null -> Exn.fail "neither type nor properties found for %S" name
    | _, `Null -> Exn.fail "strange type for %S" name
    | `Null, _ -> Exn.fail "strange properties for %S" name
    | _, _ -> Exn.fail "both type and properties found for %S" name
  in
  iter false 0 (name,mapping)

let convert_wire_type = function
| `Int -> sprintf "string_of_int %s"
| `Int64 -> sprintf "Int64.to_string %s"
| `String -> sprintf "Json.to_string (`String %s)"
| `Double -> sprintf "Json.to_string (`Double %s)"
| `Bool -> sprintf "string_of_bool %s"

let convertor (t:var_type option) unwrap name =
  match t with
  | None -> sprintf "Json.to_string %s" (unwrap name)
  | Some {multi;typ;ref=_} ->
  match multi with
  | One -> convert_wire_type typ (unwrap name)
  | Many ->
    let mapper =
      sprintf @@ match typ with
      | `Int -> "`Int %s"
      | `Int64 -> "`String (Int64.to_string %s)"
      | `String -> "`String %s"
      | `Double -> "`Double %s"
      | `Bool -> "`Bool %s"
    in
    sprintf "Json.to_string (`List (List.map (fun x -> %s) %s))" (mapper @@ unwrap "x") name

let derive mapping json =
  let query = Query.extract json in
  let (vars,json,http) =
    match query with
    | Search { q; extra; filter=_ } ->
      let c1 = List.concat @@ fst @@ List.split @@ Aggregations.analyze json in
      let c2 = Query.infer' extra q in
      let vars = Query.resolve_constraints mapping (c1 @ c2) in
      let json =
        match json with
        | `Assoc l -> `Assoc (List.map (function "query",_ -> "query", q.json | x -> x) l)
        | _ -> assert false
      in
      vars, json, ("`POST","[__esgg_index;\"_search\"]","[]",Some json)
    | Mget ids -> Query.resolve_mget_types ids, json, ("`POST","[__esgg_index;\"_mget\"]","[]",Some json)
    | Get (id,(excludes,includes)) ->
      let args =
        ["_source_include",includes; "_source_exclude",excludes]
        |> List.filter_map (function (_,None) -> None | (k,Some v) -> Some (k, String.concat "," v))
      in
      let http = ("`GET",sprintf "[__esgg_index;__esgg_kind;%s]" id.name,Stre.list (uncurry @@ sprintf "%S,%S") args,None) in (* assuming name *)
      Query.resolve_get_types id, json, http
  in
  let var_unwrap name =
    match Hashtbl.find vars name with
    | exception _ -> id
    | Property (_,es_name,_) -> sprintf "(%s.unwrap %s)" (ES_name.to_ocaml es_name)
    | Any | Type _ | List _ -> id
  in
  let var_type name : var_type option =
    match Hashtbl.find vars name with
    | Property (multi,name,typ) -> Some { multi; ref = Some name; typ; }
    | exception _ -> None
    | Any -> None
    | List typ -> Some { multi = Many; ref = None; typ; }
    | Type typ -> Some { multi = One; ref = None; typ; }
  in
  let map name = convertor (var_type name) (var_unwrap name) name in
  let (bindings,groups) = Tjson.vars json in
  let bindings = bindings |> List.map begin fun (var:Tjson.var) ->
    var.name, ((if var.optional then `Optional else `Required), `Simple (var_type var.name))
  end in
  let groups = groups |> List.map begin fun {Tjson.label;vars} ->
    let v =
      match vars with
      | [var] -> `Simple (var_type var)
      | _ -> `Group (List.map (fun v -> v, (`Required, `Simple (var_type v))) vars)
    in
    label, (`Optional, v)
  end in
  query, bindings @ groups, map, http
