open Prelude
open Printf

module Json = Yojson.Safe
module U = Json.Util

type result_type = [
  | `List of result_type
  | `Dict of (string * result_type) list
  | `Int
  | `String
  | `Double
  ]

type single_aggregation = { name : string; agg_type : string; field : string }
type aggregation = { this : single_aggregation; sub : aggregation list; }

let analyze_single_aggregation name agg_type json =
  let field = json |> U.member "field" |> U.to_string in
  { name; agg_type; field }

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
  let (sub,rest) = extract_aggregations x in
  match rest with
  | [agg_type,x] ->
    let this = analyze_single_aggregation name agg_type x in
    let sub = List.map aggregation sub in
    { this; sub }
  | _ -> Exn.fail "no aggregation?"

let get_aggregations x =
  let open U in
  extract_aggregations x |> fst |> List.map aggregation

let infer_single_aggregation { name; agg_type; field; } sub =
  let buckets t = `Dict [ "buckets", `List (sub ["key", t; "doc_count", `Int]) ] in
  let (cstr,shape) =
    match agg_type with
    | "max" | "min" | "avg" -> [], sub [ "value", `Typeof field ]
    | "terms" -> [], buckets (`Typeof field)
    | "histogram" -> [`Is (`Typeof field, `Num)], buckets `Double
    | _ -> Exn.fail "unknown agg_type %S" agg_type
  in
  cstr, (name, shape)

let rec infer_aggregation { this; sub } =
  let (constraints, subs) = List.split @@ List.map infer_aggregation sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single_aggregation this sub in
  List.flatten (cstr::constraints), desc

let analyze_aggregations query = List.map infer_aggregation (get_aggregations query)

let atd_of_es_type = function
| "long" -> `Int
| "keyword" -> `String
| s -> Exn.fail "atd_of_es_type: cannot handle %S" s

let rec resolve_types properties shape : result_type =
  let typeof t =
    match List.assoc t properties with
    | exception _ -> Exn.fail "no field %S in schema" t
    | a -> atd_of_es_type @@ U.(a |> member "type" |> to_string)
  in
  let rec map = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Typeof x -> typeof x
  | `Int | `String | `Double as t -> t
  in
  map shape

let newname =
  let nr = ref 0 in
  fun () -> incr nr; sprintf "t%d" !nr

module Gen = struct

  open Atd_ast
  let loc = dummy_loc
  let list t = `List (loc,t,[])
  let record fields = `Record (loc,fields,[])
  let field n t = `Field (loc, (n, `Required, []), t)
  let pname t params = `Name (loc,(loc,t,params),[])
  let tname t = pname t []
  let tvar t = `Tvar (loc,t)
  let ptyp name params t = `Type (loc, (name,params,[]), t)
  let typ name t = ptyp name [] t

end

let atd_of_shape name (shape:result_type) =
  let open Gen in
  let types = ref [
    ptyp "buckets" ["a"] (record [field "buckets" (list (tvar "a"))]);
    ptyp "doc_count" ["key"] (record [field "key" (tvar "key"); field "doc_count" (tname "int")]);
  ] in
  let ref_name t =
    let name = newname () in
    tuck types (typ name t);
    tname name
  in
  let rec map ?(push=ref_name) shape =
    match shape with
    | `List t -> list (map t)
    | `Int -> tname "int"
    | `String -> tname "string"
    | `Double -> tname "float"
    | `Dict ["key",k; "doc_count", `Int] -> pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> pname "buckets" [map t]
    | `Dict fields -> push @@ record (List.map (fun (n,t) -> field n (map t)) fields)
  in
  tuck types (typ name (map ~push:id shape));
  (loc,[]), List.rev !types

let derive_atd shape =
  print_endline @@ Easy_format.Pretty.to_string @@ Atd_print.format @@ atd_of_shape "result" shape

let derive mapping query =
  let properties = U.(mapping |> member "properties" |> to_assoc) in
  let result = `Dict ["aggregations", `Dict (List.map snd @@ analyze_aggregations query)] in (* XXX discarding constraints *)
  derive_atd @@ resolve_types properties result;
  ()
