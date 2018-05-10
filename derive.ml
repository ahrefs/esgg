open ExtLib
open Prelude
open Printf

open Common

type result_type = [
  | `List of result_type
  | `Dict of (string * result_type) list
  | `Assoc of (result_type * result_type)
  | ref_type
  | nullable_type
  ]

type agg_type =
| Simple_metric of string
| Cardinality of string
| Terms of { field : string; size : Tjson.t }
| Histogram of string
| Date_histogram of string
| Filter
| Filters
| Top_hits
| Range of string
| Nested of string
| Reverse_nested

type single_aggregation = { name : string; agg : agg_type; }
type aggregation = { this : single_aggregation; sub : aggregation list; }

let analyze_single_aggregation name agg_type json =
  let field () = U.(get json "field" to_string) in
  let agg =
    match agg_type with
    | "max" | "min" | "avg" | "sum" -> Simple_metric (field ())
    | "cardinality" -> Cardinality (field ())
    | "terms" | "significant_terms" -> Terms { field = field (); size = U.member "size" json }
    | "histogram" -> Histogram (field ())
    | "date_histogram" -> Date_histogram (field ())
    | "filter" -> Filter
    | "filters" -> Filters
    | "top_hits" -> Top_hits
    | "range" -> Range (field ()) (* TODO keyed *)
    | "nested" -> Nested U.(get json "path" to_string)
    | "reverse_nested" -> Reverse_nested
    | _ -> Exn.fail "unknown aggregation type %S" agg_type
  in
  { name; agg; }

let extract_aggregations x =
  let open U in
  let (aggs,rest) = List.partition (function (("aggregations"|"aggs"),_) -> true | _ -> false) (to_assoc x) in
  let aggs =
    match aggs with
    | [] -> []
    | (_,a) :: [] -> to_assoc a
    | _::_::_ -> Exn.fail "only one aggregation expected"
  in
  aggs,rest

let rec aggregation (name,x) =
  try
    let (sub,rest) = extract_aggregations x in
    match rest with
    | [agg_type,x] ->
      let this = analyze_single_aggregation name agg_type x in
      let sub = List.map aggregation sub in
      { this; sub }
    | _ -> Exn.fail "no aggregation?"
  with
    exn -> Exn.fail ~exn "aggregation %S" name

let get_aggregations x =
  extract_aggregations x |> fst |> List.map aggregation

let infer_single_aggregation { name; agg; } sub =
  let buckets ?(extra=[]) t = `Dict [ "buckets", `List (sub @@ ("key", t) :: ("doc_count", `Int) :: extra) ] in
  let (cstr,shape) =
    match agg with
    | Simple_metric field -> [`Is_num field], sub [ "value", `Maybe `Double ]
    | Cardinality _field -> [], sub ["value", `Int ]
    | Terms { field; size } -> (match size with `Var var -> [`Var (`Int, var)] | _ -> []), buckets (`Typeof field)
    | Histogram field -> [`Is_num field], buckets `Double
    | Date_histogram field -> [`Is_date field], buckets `Int ~extra:["key_as_string", `String]
    | Filter | Nested _ | Reverse_nested -> [], sub ["doc_count", `Int]
    | Filters -> [], `Dict [ "buckets", `Assoc (`String, sub ["doc_count", `Int])]
    | Top_hits -> [], `Dict [ "hits", `Dict [ "total", `Int ] ]
    | Range field -> [`Is_num field], `Dict [ "buckets", `List (sub ["doc_count", `Int]) ]
  in
  cstr, (name, shape)

let rec infer_aggregation { this; sub } =
  let (constraints, subs) = List.split @@ List.map infer_aggregation sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single_aggregation this sub in
  List.flatten (cstr::constraints), desc

let analyze_aggregations query = List.map infer_aggregation (get_aggregations query)

let resolve_types mapping shape : result_type =
  let rec map = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Assoc (k,v) -> `Assoc (map k, map v)
  | `Typeof x -> let name = ES_name.make mapping x in `Ref (name, typeof mapping name)
  | `Maybe _ | `Int64 | `Int | `String | `Double as t -> t
  in
  map shape

let newname =
  let nr = ref 0 in
  fun () -> incr nr; sprintf "t%d" !nr

module Gen = struct

  open Atd_ast
  let loc = dummy_loc
  let annot section l = (section, (loc, List.map (fun (k,v) -> k, (loc, Some v)) l))
  let annots a = Atd_annot.merge @@ List.map (fun (s,l) -> annot s l) a
  let list ?(a=[]) t = `List (loc,t,annots a)
  let tuple l = `Tuple (loc, List.map (fun t -> (loc,t,[])) l, [])
  let record fields = `Record (loc,fields,[])
  let field ?(kind=`Required) ?(a=[]) n t = `Field (loc, (n, kind, annots a), t)
  let pname ?(a=[]) t params = `Name (loc,(loc,t,params),annots a)
  let tname ?a t = pname ?a t []
  let nullable ?(a=[]) t = `Nullable (loc,t,annots a)
  let option ?(a=[]) t = `Option (loc,t,annots a)
  let wrap ocaml t = `Wrap (loc,t,annots ["ocaml",ocaml])
  let tvar t = `Tvar (loc,t)
  let ptyp ?(a=[]) name params t = `Type (loc, (name,params,annots a), t)
  let typ ?a name t = ptyp ?a name [] t

end

let atd_of_simple_type =
  let open Gen in
  function
  | `Int -> tname "int"
  | `Int64 -> tname ~a:["ocaml",["repr","int64"]] "int"
  | `String -> tname "string"
  | `Double -> tname "float"
  | `Bool -> tname "bool"

let atd_of_nullable_type = function
| #simple_type as t -> atd_of_simple_type t
| `Maybe t -> Gen.nullable @@ atd_of_simple_type t

let wrap_ref ref t = Gen.wrap ["module",ES_name.to_ocaml ref] (atd_of_simple_type t)

let atd_of_var_type' : var_type' -> Atd_ast.type_expr = function
| #simple_type as t -> atd_of_simple_type t
| `Ref (ref,t) -> wrap_ref ref t
| `List (`Ref (ref,t)) -> Gen.list (wrap_ref ref t)
| `List (#simple_type as t) -> Gen.list (atd_of_simple_type t)
| `Json -> Gen.tname "basic_json"

let atd_of_var_type : var_type -> Atd_ast.type_expr = function
| #var_type' as t -> atd_of_var_type' t
| `Optional t -> Gen.option @@ atd_of_var_type' t

let atd_name name =
  match name with
  | "type" -> ["json",["name",name]],"type_"
  | s -> [],s

let atd_of_shape name (shape:result_type) =
  let open Gen in
  let names = Hashtbl.create 10 in
  let types = ref [] in
  let fresh_name t =
    let (prefix,start) =
      match t with
      | `Record (_,[`Field (_,(name,_,_),_)],_) -> name, name
      | _ -> "t", "t0"
    in
    let rec loop name n =
      let (_,name) (* TODO *) = atd_name name in
      match Hashtbl.mem names name with
      | true -> loop (sprintf "%s%d" prefix n) (n+1)
      | false -> name
    in
    loop start 1
  in
  let new_type typ =
    tuck types typ;
    let `Type (_,(name,_,_),_) = typ in
    assert (not @@ Hashtbl.mem names name);
    Hashtbl.add names name ()
  in
  new_type @@ ptyp "doc_count" ["key"] (record [field "key" (tvar "key"); field "doc_count" (tname "int")]);
  new_type @@ ptyp "buckets" ["a"] (record [field "buckets" (list (tvar "a"))]);
  let ref_name t =
    match List.find (fun (`Type (_,_,v)) -> t = v) !types with
    | `Type (_,(name,[],_),_) -> tname name
    | _ -> assert false (* parametric type cannot match *)
    | exception _ ->
      let name = fresh_name t in
      new_type @@ typ name t;
      tname name
  in
  let rec map ?push shape = snd @@ map' ?push shape
  and map' ?(push=ref_name) shape =
    match shape with
    | #nullable_type as c -> [], atd_of_nullable_type c
    | `Ref (ref,t) -> ["doc",["text",ES_name.show ref]], wrap_ref ref t
    | `List t -> [], list (map t)
    | `Assoc (k,v) -> [], list ~a:["json",["repr","object"]] (tuple [map k; map v])
    | `Dict ["key",k; "doc_count", `Int] -> [], pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> [], pname "buckets" [map t]
    | `Dict fields ->
      let fields = fields |> List.map begin fun (name,t) ->
        let kind = match t with `Maybe _ -> `Optional | `List _ -> `With_default | _ -> `Required in
        let (a,t) = map' t in
        let (a',name) = atd_name name in
        let a = a' @ a in
        field ~a ~kind name t
      end in
      [], push @@ record fields
  in
  tuck types (typ name (map ~push:id shape));
  (loc,[]), List.rev !types

let shape_of_mapping x : result_type =
  let rec make path json =
    let meta = `Assoc (Option.default [] @@ U.(opt json "_meta" to_assoc)) in
    let maybe_multi ?(default=false) t =
      let multi = Option.default default U.(opt meta "multi" to_bool) in
      if multi then `List t else t
    in
    (* can have only simple optional type now, thats why
    TODO lift restriction *)
    let maybe_optional (t:simple_type) =
      let multi = Option.default false U.(opt meta "multi" to_bool) in
      let optional = Option.default false U.(opt meta "optional" to_bool) in
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
    match U.(get json "properties" to_assoc) with
    | exception _ -> Exn.fail "strange mapping : %s" (U.to_string json)
    | f -> `Dict (f |> List.map (fun (name,x) -> name, make (ES_name.append path name) x))
  in
  make (ES_name.make x "") x.mapping

let output mapping query =
  let shape =
    match U.assoc "query" query with
    | exception _ ->
      let source = shape_of_mapping mapping in
      `Dict ["docs",`List (`Dict ["_id", `String; "found", `Bool; "_source", source])]
    | _ ->
      let aggs = List.map snd @@ analyze_aggregations query in (* XXX discarding constraints *)
      let result = `Dict (("hits", `Dict ["total", `Int]) :: (if aggs = [] then [] else ["aggregations", `Dict aggs])) in
      resolve_types mapping result
  in
  atd_of_shape "result" shape

let atd_of_vars l =
  let open Gen in
  let basic_json =
    if List.exists (fun (_,t) -> t = `Json) l then
      [typ "basic_json" ~a:["ocaml",["module","Json";"t","json"]] (tname "abstract")]
    else
      []
  in
  let input = [typ "input" (record (List.map (fun (n,t) -> field n (atd_of_var_type t)) l))] in
  (loc,[]), (basic_json @ input)

let uident s =
  assert (s <> "");
  let s = String.map (function ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c) -> c | _ -> '_') s in
  let s = match s.[0] with '0'..'9' -> "M_" ^ s | _ -> s in
  String.capitalize s

let print_reflect name mapping =
  let extern name = name ^ "_" in
  let rec iter hash nr_indent (name,x) =
    let indent = String.make nr_indent ' ' in
    let hash = hash || String.exists name "hash" in
    match U.member "type" x, U.member "properties" x with
    | `String typ, `Null ->
      let typ = if typ = "long" && hash then "int64" else typ in
      printfn "%smodule %s = %s(%s)" indent (uident name) (extern "Id") (uident @@ extern typ)
    | (`Null | `String "nested"), `Assoc props ->
      let modul = uident name in
      printfn "%smodule %s = struct" indent modul;
      List.iter (iter hash (nr_indent+2)) props;
      printfn "%send (* %s *)" indent modul
    | `Null, `Null -> Exn.fail "neither type nor properties found for %S" name
    | _, `Null -> Exn.fail "strange type for %S" name
    | `Null, _ -> Exn.fail "strange properties for %S" name
    | _, _ -> Exn.fail "both type and properties found for %S" name
  in
  iter false 0 (name,mapping)

let () = Printexc.register_printer (function Failure s -> Some s | _ -> None)
