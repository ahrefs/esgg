open ExtLib
open Prelude
open Printf

open Common

let resolve_types mapping shape : result_type =
  let rec map = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Assoc (k,v) -> `Assoc (map k, map v)
  | `Typeof x -> let name = ES_name.make mapping x in `Ref (name, typeof mapping name)
  | `Maybe _ | `Int64 | `Int | `String | `Double as t -> t
  in
  map shape

let shape_of_mapping x : result_type =
  let module S = Set.Make(ES_name) in
  let smake k = try U.(x.mapping |> member "_source" |> get k (to_list to_string)) |> List.map (ES_name.make x) |> S.of_list |> some  with _ -> None in
  let excludes = smake "excludes" in
  let includes = smake "includes" in
  let rec make path json =
    let meta = `Assoc (Option.default [] @@ U.(opt "_meta" to_assoc json)) in
    let maybe_multi ?(default=false) t =
      let multi = Option.default default U.(opt "multi" to_bool meta) in
      if multi then `List t else t
    in
    (* can have only simple optional type now, thats why
    TODO lift restriction *)
    let maybe_optional (t:simple_type) =
      let multi = Option.default false U.(opt "multi" to_bool meta) in
      let optional = Option.default false U.(opt "optional" to_bool meta) in
      if optional then
        `Maybe t
      else
        if multi then `List (t:>result_type) else (t:>result_type)
    in
    match U.assoc "type" json with
    | exception _ -> maybe_multi (make_properties path json)
    | `String "nested" -> maybe_multi ~default:true (make_properties path json)
    | `String t -> maybe_optional (simple_of_es_type path t)
    | _ -> Exn.fail "strange type : %s" (U.to_string json)
  and make_properties path json =
    match U.(get "properties" to_assoc json) with
    | exception _ -> Exn.fail "strange mapping : %s" (U.to_string json)
    | f -> `Dict (f |> List.filter_map begin fun (name,x) ->
      let path = ES_name.append path name in
      let included = (* TODO wildcards *)
        (match excludes with None -> true | Some set -> not @@ S.mem path set) &&
        (match includes with None -> true | Some set -> S.mem path set)
      in
      match included with
      | false -> None
      | true -> Some (name, make path x)
      end)
  in
  make (ES_name.make x "") x.mapping

let output mapping query =
  let shape =
    match U.assoc "query" query with
    | exception _ ->
      let source = shape_of_mapping mapping in
      `Dict ["docs",`List (`Dict ["_id", `String; "found", `Bool; "_source", source])]
    | _ ->
      let aggs = List.map snd @@ Aggregations.analyze query in (* XXX discarding constraints *)
      let result = `Dict (("hits", `Dict ["total", `Int]) :: (if aggs = [] then [] else ["aggregations", `Dict aggs])) in
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
  let (vars,json) =
    match Query.extract json with
    | Search q ->
      let c1 = List.concat @@ fst @@ List.split @@ Aggregations.analyze json in
      let c2 = Query.infer q in
      let vars = Query.resolve_constraints mapping (c1 @ c2) in
      let json =
        match json with
        | `Assoc l -> `Assoc (List.map (function "query",_ -> "query", q.json | x -> x) l)
        | _ -> assert false
      in
      vars, json
    | Mget ids -> Query.resolve_mget_types ids, json
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
  let vars = Tjson.vars ~optional:true json |> List.map begin fun (var:Tjson.var) ->
    var.name, ((if var.optional then `Optional else `Required), var_type var.name)
  end in
  vars, map, json
