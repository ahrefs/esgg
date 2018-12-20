open Devkit
open ExtLib

open Common

type agg_type =
| Simple_metric of [`MinMax | `Avg | `Sum ] * value
| Cardinality of value
| Terms of { term : value; size : Tjson.t }
| Histogram of value
| Date_histogram of value
| Filter of Query.query
| Filters of (string * Query.query) list
| Filters_dynamic
| Top_hits of source_filter option
| Range of value
| Range_keyed of value * string list
| Nested of string
| Reverse_nested of string option

type single = { name : string; agg : agg_type; }
type t = { this : single; sub : t list; }

let analyze_single name agg_type json =
  let value () =
    match U.member "field" json with
    | `String s -> Field s
(*     | `Var v -> Variable v *)
    | `Null ->
      begin match U.member "inline" @@ U.member "script" json with
      | `String s -> Script (Static s) (* TODO parse painless *)
      | `Var v -> Script (Dynamic v)
      | exception exn -> Exn.fail ~exn "failed to get aggregation field"
      | _ -> Exn.fail "expected string as inline script"
      end
    | _ -> Exn.fail "bad aggregation field"
  in
  let (json, agg) =
    match agg_type with
    | "filter" -> let q = Query.extract_query json in q.json, Filter q
    | "filters" ->
      begin match json |> U.member "filters" with
      | `Assoc a ->
        let filters = List.map (fun (k,v) -> k, Query.extract_query v) a in
        let json = Tjson.replace json "filters" (`Assoc (List.map (fun (k,q) -> k, q.Query.json) filters)) in
        json, Filters filters
      | `Var _ -> json, Filters_dynamic
      | _ -> Exn.fail "filters: expecting either dict or variable"
      end
    | _ ->
    json,
    match agg_type with
    | "max" | "min" -> Simple_metric (`MinMax, value ())
    | "sum" -> Simple_metric (`Sum, value ())
    | "avg" -> Simple_metric (`Avg, value ())
    | "cardinality" -> Cardinality (value ())
    | "terms" | "significant_terms" -> Terms { term = value (); size = U.member "size" json }
    | "histogram" -> Histogram (value ())
    | "date_histogram" -> Date_histogram (value ())
    | "top_hits" -> Top_hits (Query.extract_source json)
    | "range" when U.(opt "keyed" to_bool json) = Some true ->
      let keys = U.(get "ranges" (to_list (get "key" to_string))) json in
      Range_keyed (value (), keys)
    | "range" -> Range (value ())
    | "nested" -> Nested U.(get "path" to_string json)
    | "reverse_nested" -> Reverse_nested U.(opt "path" to_string json)
    | _ -> Exn.fail "unknown aggregation type %S" agg_type
  in
  json, { name; agg; }

let extract x =
  let open U in
  let (aggs,rest) = List.partition (function (("aggregations"|"aggs"),_) -> true | _ -> false) (to_assoc x) in
  let aggs =
    match aggs with
    | [] -> []
    | (_,a) :: [] -> to_assoc a
    | _::_::_ -> Exn.fail "only one aggregation expected"
  in
  aggs,rest

let rec make (name,x) =
  try
    let (sub,rest) = extract x in
    match rest with
    | [agg_type,x] ->
      let json, this = analyze_single name agg_type x in
      let sub = List.map make sub in
      let sub_json = match sub with [] -> [] | _ -> ["aggregations", `Assoc (List.map (fun (j, agg) -> agg.this.name, j) sub)] in
      let json = `Assoc ((agg_type, json) :: sub_json) in
      json, { this; sub = List.map snd sub }
    | _ -> Exn.fail "no aggregation?"
  with
    exn -> Exn.fail ~exn "aggregation %S" name

let get x =
  let sub = extract x |> fst |> List.map make in
  `Assoc (List.map (fun (j, agg) -> agg.this.name, j) sub), List.map snd sub

let infer_single mapping ~nested { name; agg; } sub =
  let buckets ?(extra=[]) t = `Dict [ "buckets", `List (sub @@ ("key", t) :: ("doc_count", `Int) :: extra) ] in
  let doc_count () = sub ["doc_count", `Int] in
  let keyed_buckets keys =
    let d = doc_count () in
    `Dict [ "buckets", `Dict (List.map (fun k -> k, d) keys)]
  in
  let (cstr,shape) =
    match agg with
    | Simple_metric (metric, value) ->
      let value_type = (typeof_ mapping value :> resolve_type) in
      let typ =
        match metric with
(*         | `MinMax -> `Maybe (`Typeof value) *)
        | `MinMax -> `Maybe value_type (* TODO use Typeof, but need meta annotation to fallback to value_type *)
        | `Avg -> `Maybe `Double
        | `Sum ->
          match value_type with
          | `Bool -> `Int
          | `Int | `Int64 -> `Dict ["override int as float hack", `Int]
          | _ -> value_type
      in
      [], sub [ "value", typ ]
    | Cardinality _value -> [], sub ["value", `Int ]
    | Terms { term; size } -> (match size with `Var var -> [On_var (var, Eq_type `Int)] | _ -> []), buckets (`Typeof term)
    | Histogram value -> [Field_num value], buckets `Double
    | Date_histogram value -> [Field_date value], buckets `Int ~extra:["key_as_string", `String]
    | Nested _ | Reverse_nested _ -> [], doc_count ()
    | Filter q -> Query.infer q, doc_count ()
    | Filters l ->  (* TODO other_bucket *)
      let cstrs = l |> List.map snd |> List.map Query.infer |> List.flatten in
      cstrs, keyed_buckets (List.map fst l)
    | Filters_dynamic -> (* by convention assume dynamic filters will be an assoc and so output will be assoc too *)
      [], `Dict [ "buckets", `Object (doc_count ()) ]
    | Top_hits source -> [], `Dict [ "hits", sub ((Hit.hits_ mapping ~highlight:None (*?*) ?nested source) :> (string * resolve_type) list) ]
    | Range value -> [Field_num value], buckets `String
    | Range_keyed (value,keys) -> [Field_num value], keyed_buckets keys
  in
  cstr, (name, shape)

let rec infer mapping ~nested { this; sub } =
  let nested =
    match this.agg with
    | Nested path -> Some (ES_name.make mapping path)
    | Reverse_nested path -> (match path with Some path -> Some (ES_name.make mapping path) | None -> None)
    | _ -> nested
  in
  let (constraints, subs) = List.split @@ List.map (infer mapping ~nested) sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single mapping ~nested this sub in
  List.flatten (cstr::constraints), desc

let analyze mapping query =
  let (json,sub) = get query in
  json, List.map (infer mapping ~nested:None) sub
