open Prelude
open Printf

module Json = Yojson.Safe
module U = Json.Util

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
  let buckets () = `Dict [ "buckets", `List (sub ["key", `Typeof field; "doc_count", `Int]) ] in
  let (cstr,shape) =
    match agg_type with
    | "max" | "min" | "avg" -> [], sub [ "value", `Typeof field ]
    | "terms" -> [], buckets ()
    | "histogram" -> [`Is (`Typeof field, `Num)], buckets ()
    | _ -> Exn.fail "unknown agg_type %S" agg_type
  in
  cstr, (name, shape)

let rec infer_aggregation { this; sub } =
  let (constraints, subs) = List.split @@ List.map infer_aggregation sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single_aggregation this sub in
  List.flatten (cstr::constraints), desc

let analyze query = List.map infer_aggregation (get_aggregations query)

let atd_of_es_type = function
| "long" -> `Int
| "keyword" -> `String
| s -> Exn.fail "atd_of_es_type: cannot handle %S" s

let rec resolve_types properties shape =
  let typeof t =
    match List.assoc t properties with
    | exception _ -> Exn.fail "no field %S in schema" t
    | a -> atd_of_es_type @@ U.(a |> member "type" |> to_string)
  in
  let rec map = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Typeof x -> typeof x
  | `Int as t -> t
  in
  map shape

let newname =
  let nr = ref 0 in
  fun () -> incr nr; sprintf "helper%02d" !nr

let derive_atd (name,shape) =
  let open Atd_ast in
  let loc = dummy_loc in
  let list t = `List (loc,t,[]) in
  let record fields = `Record (loc,fields,[]) in
  let field n t = `Field (loc, (n, `Required, []), t) in
  let tname t = `Name (loc,(loc,t,[]),[]) in
  let typ name t = `Type (loc, (name,[],[]), t) in
  let types = ref [] in
  let rec map shape =
    match shape with
    | `List t -> list (map t)
    | `Int -> tname "int"
    | `Dict fields ->
      let name = newname () in
      tuck types (typ name (record (List.map (fun (n,t) -> field n (map t)) fields)));
      tname name
    | `Typeof x -> tname "int"
  in
  tuck types (typ name (map shape));
  List.rev !types

let derive_atds query =
  let types = List.concat @@ List.map derive_atd @@ List.map snd @@ analyze query in
  let atd = (Atd_ast.dummy_loc,[]),types in
  print_endline @@ Easy_format.Pretty.to_string @@ Atd_print.format atd

let derive mapping query =
  let _properties = U.(mapping |> member "properties" |> to_assoc) in
  let _aggs = get_aggregations query in
  derive_atds query;
  ()
