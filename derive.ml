open ExtLib
open Printf

open Common

let derive_stored_fields mapping fields =
  try Dict (Aggregations.derive_fields mapping fields) with Failure s -> fail "derive_stored_fields: %s" s

let generate_inner_hits mapping full_source inner_hits_list =
  match inner_hits_list with
  | [] -> None
  | specs ->
    let inner_hits_dict = specs |> List.map (fun (path, spec) ->
      let nested_name = ES_name.make mapping path in
      let highlight = Option.map (Aggregations.derive_highlight mapping) spec.highlight in
      let fields = Option.map (derive_stored_fields mapping) spec.fields in
      let source_type =
        match spec.source with
        | None -> full_source
        | Some filter -> Some (Static filter)
      in
      let key = Option.default path spec.name in
      key, Hit.inner_hits_result mapping ~nested:nested_name ~highlight ?fields source_type
    ) in
    Some (Maybe (Dict inner_hits_dict))

let output mapping query =
  match Query.extract query with
  | Get { id = _; return = `Source filter; } -> Hit.doc @@ Maybe (Hit.of_mapping ~filter mapping)
  | Get { id = _; return = `Fields fields; } ->
    let fields = derive_stored_fields mapping fields in
    Hit.doc_no_source ~fields ()
  | Get { id = _; return = `Nothing; } -> Hit.doc_no_source ()
  | Mget { conf; _ } ->
    begin match Query.extract_source_static conf with (* should be extracted in Query.extract *)
      | None -> Dict ["docs",List (Hit.doc_no_source ())]
      | Some filter -> Dict ["docs",List (Hit.doc @@ Maybe (Hit.of_mapping ~filter mapping))]
    end
  | Search { q; source; highlight; fields; _ } ->
    let highlight = Option.map (Aggregations.derive_highlight mapping) highlight in
    let fields = Option.map (derive_stored_fields mapping) fields in
    let inner_hits_specs = Query.extract_inner_hits_from_query q in
    let inner_hits_type = generate_inner_hits mapping source inner_hits_specs in
    let matched_queries = if Query.has_matched_queries query then Some (Maybe (List (Simple String))) else None in
    let hits = Hit.hits mapping ?inner_hits:inner_hits_type ~highlight ?fields ?matched_queries source in
    let aggs = List.map snd @@ snd @@ Aggregations.analyze mapping query in (* XXX discarding constraints *)
    Dict (("hits", hits) :: (if aggs = [] then [] else ["aggregations", Dict aggs]))

let print_reflect name mapping =
  let extern name = name ^ "_" in
  let rec iter nr_indent (name,x) =
    let indent = String.make nr_indent ' ' in
    let meta = get_meta x in
    let repr = get_repr_opt meta in
    match U.member "type" x, U.member "properties" x with
    | `String typ, `Null ->
      let typ = Option.default typ repr in
      printfn "%smodule %s = %s(%s)" indent (to_valid_modname name) (extern "Id") (to_valid_modname @@ extern typ)
    | (`Null | `String "nested"), `Assoc props ->
      let modul = to_valid_modname name in
      printfn "%smodule %s = struct" indent modul;
      List.iter (iter (nr_indent+2)) props;
      printfn "%send (* %s *)" indent modul
    | `Null, `Null -> fail "neither type nor properties found for %S" name
    | _, `Null -> fail "strange type for %S" name
    | `Null, _ -> fail "strange properties for %S" name
    | _, _ -> fail "both type and properties found for %S" name
  in
  iter 0 (name,mapping)

let convert_wire_type = function
| Int -> sprintf "string_of_int %s"
| Int64 -> sprintf "Int64.to_string %s"
| String | Date -> sprintf "Json.to_string (`String %s)"
| Double -> sprintf "Json.to_string (`Float %s)"
| Bool -> sprintf "string_of_bool %s"
| Json -> sprintf "Json.to_string %s"

let map_wire_type typ =
  sprintf @@ match typ with
  | Int -> "`Int %s"
  | Int64 -> "`String (Int64.to_string %s)"
  | String | Date -> "`String %s"
  | Double -> "`Float %s"
  | Bool -> "`Bool %s"
  | Json -> "%s"

let convertor (t:var_type option) unwrap name =
  match t with
  | None -> sprintf "Json.to_string %s" (unwrap name)
  | Some {cardinality;typ;ref=_} ->
  match cardinality with
  | One -> convert_wire_type typ (unwrap name)
  | Many -> sprintf "Json.to_string (`List (List.map (fun x -> %s) %s))" (map_wire_type typ (unwrap "x")) name

(** Turn source related fields from json into query parameters. *)
let source_args source =
  match source with
  | None -> []
  | Some {excludes;includes} ->
    ["_source_includes",includes; "_source_excludes",excludes]
    |> List.filter_map (function (_,None) -> None | (k,Some v) -> Some (k, String.concat "," v))

let args_to_ocaml_string args =
  sprintf "[%s]" @@ String.concat ";" @@ List.map (fun (a,b) -> sprintf "%S,%S" a b) args

let derive mapping json =
  let query = Query.extract json in
  let (vars,json,http) =
    match query with
    | Search { q; extra; source=_; highlight=_; fields=_; } ->
      let (agg_json, aggs) = Aggregations.analyze mapping json in
      let c1 = List.concat @@ fst @@ List.split aggs in
      let c2 = Query.infer' extra q in
      let vars = Query.resolve_constraints mapping (c1 @ c2) in
      let json = Tjson.(remove "query" @@ remove "aggregations" @@ remove "aggs" @@ remove "_esgg" json) in
      (* handle optional variables outside of aggregation and query parts *)
      let json = Tjson.(map (function `Var var when var.optional -> `Optional ({label=var.name;vars=[var.name]}, `Var var) | x -> x) json) in
      let json = Tjson.add json ["query",q.json; "aggregations", agg_json] in
      vars, json, ("`POST","[__esgg_index;\"_search\"]","[]",Some json)
    | Mget { ids; json; conf } ->
      let args = conf |> Query.extract_source_static |> source_args |> args_to_ocaml_string in
      Query.resolve_mget_types ids, json, ("`POST","[__esgg_index;\"_mget\"]",args,Some json)
    | Get { id; return; } ->
      let args =
        match return with
        | `Nothing -> [ "_source", "false" ]
        | `Source source -> source_args @@ Some source
        | `Fields fields -> [ "stored_fields", String.concat "," fields ]
      in
      let http = ("`GET",sprintf "[__esgg_index;__esgg_kind;%s]" id.name, args_to_ocaml_string args, None) in (* assuming name *)
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
    | Property (cardinality,name,typ) -> Some { cardinality; ref = Some name; typ; }
    | exception _ -> None
    | Any -> None
    | List typ -> Some { cardinality = Many; ref = None; typ; }
    | Type typ -> Some { cardinality = One; ref = None; typ; }
  in
  let check_cardinality v t =
    match t with
    | Some t when t.cardinality = One && v.Tjson.list -> fail "var %S marked as list, but derived otherwise" v.name
    | _ -> ()
  in
  let map name = convertor (var_type name) (var_unwrap name) name in
  let (bindings,groups) = Tjson.vars json in
  let bindings = bindings |> List.map begin fun (var:Tjson.var) ->
    let t = var_type var.name in
    check_cardinality var t;
    var.name, ((if var.optional then `Optional else `Required), `Simple t)
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
