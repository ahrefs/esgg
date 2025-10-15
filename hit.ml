open ExtLib

open Common

module ES_names = Set.Make(ES_name)
let es_names mapping l = l |> List.map (ES_name.make mapping) |> ES_names.of_list

let option_map2 op a b =
  match a, b with
  | x, None | None, x -> x
  | Some a, Some b -> Some (op a b)

let include_parents x = ES_names.fold (ES_name.fold_up ES_names.add) x x
let parent_included path set = ES_name.fold_up (fun x acc -> acc || ES_names.mem x set) path false

let debug = false

let of_mapping ?(filter=empty_filter) x =
  let smake k = source_fields k x.mapping |> Option.map (es_names x) in
  let excludes = option_map2 ES_names.union (smake "excludes") (Option.map (es_names x) filter.excludes) in
  let includes = option_map2 ES_names.inter (smake "includes") (Option.map (es_names x) filter.includes) in
  if debug then Option.may (ES_names.iter (fun s -> printfn "esname %s" (ES_name.to_ocaml s))) includes;
  let includes = match includes with None -> None | Some set -> Some (set, include_parents set) in
  if debug then Option.may (fun (_,x) -> x |> ES_names.iter (fun s -> printfn "parent esname %s" (ES_name.to_ocaml s))) includes;
  let get_bool meta k = U.(opt k to_bool meta) in
  let rec make ~optional ~name path json =
    if debug then printfn "make %s %S" name (ES_name.show path);
    let meta = get_meta json in
    let flag = get_bool meta in
    let repr = get_repr_opt meta in
    let wrap default_list t =
      let list =
        match U.member "list" meta, U.member "multi" meta with
        | `Null, `Null -> `Bool default_list
        | `Null, x -> x
        | x, _ -> x
      in
      let t =
        match list with
        | `Bool true -> List t
        | `Bool false -> t
        | `String "sometimes" -> List_or_single t
        | _ -> fail "attribute \"list\" can only be one of : true, false, \"sometimes\""
      in
      if Option.default optional @@ flag "optional" then Maybe t else t
    in
    let default_optional = Option.default false @@ flag "fields_default_optional" in
    let t =
      match repr, U.assoc "type" json with
      | exception _ -> wrap false @@ make_properties ~default_optional path json
      | _, `String "nested" -> wrap true @@ make_properties ~default_optional path json
      | Some t, `String _ | _, `String t -> wrap false @@ Ref (path, simple_of_es_type t)
      | _ -> fail "strange type : %s" (U.to_string json)
    in
    match U.assoc "fields" json with
    | exception _ -> name, t
    | _ ->
      match make_fields ~default_optional path json with
      | [] -> name, t
      | [k,v] ->
        assert (name <> "");
        (* when field is extracted - substitude it for the current key *)
        Printf.sprintf "%s.%s" name k, v
      | l -> fail "got %d fields for %s, but can only handle one (sort of bug)" (List.length l) (ES_name.show path)
  and make_fields ~default_optional path json = make_props "fields" ~default_optional path json
  and make_properties ~default_optional path json = Dict (make_props "properties" ~default_optional path json)
  and make_props k ~default_optional path json =
    if debug then printfn "make_%s %S" k (ES_name.show path);
    match U.(get k to_assoc json) with
    | exception _ -> fail "strange mapping : %s" (U.to_string json)
    | f -> f |> List.filter_map begin fun (name,x) ->
      begin match x with `Assoc _ -> () | _ -> fail "make_%s : %S not a dict" k name end;
      let path = ES_name.append path name in
      let included = (* TODO wildcards *)
        (match excludes with
        | None -> true
        | Some set -> not @@ ES_names.mem path set)
        &&
        (* fields are not extracted by default, but only when explicitly requested by includes *)
        (match includes with
        | None -> k = "properties"
        | Some (set,parents) -> k = "properties" && parent_included path set || ES_names.mem path parents)
        &&
        not @@ Option.default false @@ get_bool (get_meta x) "ignore"
      in
      match included with
      | false -> (* printfn "(* excluded %s *)" (ES_name.show path); *) None
      | true -> Some (make ~optional:default_optional ~name path x)
      end
  in
  snd @@ make ~optional:false ~name:"" (ES_name.make x "") x.mapping

let get_nested path x =
  let rec loop path x =
    match path, x with
    | p, (Maybe x | List x) -> loop p x (* HACK unwrap *)
    | [], _ -> x
    | k::p, Dict l -> loop p (try List.assoc k l with exn -> fail ~exn "cannot find nested %S" k)
    | k::_, _ -> fail "nested %S is not a dict" k
  in
  loop (ES_name.get_path path) x

let doc_ ?(id=true) ?found ?highlight ?fields ?matched_queries ?inner_hits source =
  let a = [
    "_id", if id then Some (Simple String) else None;
  (*
    "_index", `String;
    "_type", `String;
    "_score", `Maybe `Float;
  *)
    "found", found;
    "_source", source;
    "highlight", highlight;
    "fields", fields;
    "inner_hits", inner_hits;
    "matched_queries", matched_queries;
  ] |> List.filter_map (function (_,None) -> None | (k,Some v) -> Some (k,v))
  in
  Dict a

(*
  fields are not allowed together with source
  https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-get.html
*)
let doc_no_source ?fields () = doc_ ~found:(Simple Bool) ?fields None
let doc source = doc_ ~found:(Simple Bool) (Some source)

let nested_meta () =
  Dict [
    "field", Simple String;
    "offset", Simple Int;
  ]

let inner_hit ~nested ~highlight ?fields source =
  let source_type = get_nested nested source in
  let a =
    (match highlight with None -> [] | Some h -> ["highlight", h]) @
    (match fields with None -> [] | Some f -> ["fields", f])
    @ [
    "_id", Simple String;
    "_nested", nested_meta ();
    "_source", source_type;
  ]
  in
  Dict a

let inner_hits_result mapping ~nested ~highlight ?fields source_type =
  let source_for_inner_hit =
    match source_type with
    | None -> of_mapping mapping
    | Some (Static filter) -> of_mapping ~filter mapping
    | Some (Dynamic _) -> Simple Json
  in
  Dict [
    "hits", Dict [
      "total", Simple Int;
      "hits", List (inner_hit ~nested ~highlight ?fields source_for_inner_hit);
    ]
  ]

let hit ?highlight ?id ?fields ?matched_queries ?inner_hits source = doc_ ?highlight ?id ?fields ?matched_queries ?inner_hits (Some source)

let hits_ mapping ?nested ~highlight ?fields ?matched_queries ?inner_hits source =
  let hit x =
    match nested with
    | None -> hit ?highlight ?fields ?matched_queries ?inner_hits x
    | Some nested -> hit ~id:false ?highlight ?fields ?matched_queries ?inner_hits (get_nested nested x)
  in
  List.concat [
    ["total", Simple Int];
    (match source with
    | None -> ["hits", List (hit (Simple Json))]
    | Some (Static filter) -> ["hits", List (hit (of_mapping ~filter mapping))]
    | Some (Dynamic _) -> ["hits", List (hit (Simple Json))]);
  ]

let hits mapping ?nested ~highlight ?fields ?matched_queries ?inner_hits source = Dict (hits_ mapping ?nested ~highlight ?fields ?matched_queries ?inner_hits source)
