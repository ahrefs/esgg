open ExtLib

open Common

type agg_type =
| Simple_metric of [`MinMax | `Avg | `Sum ] * string
| Cardinality of string
| Terms of { field : string; size : Tjson.t }
| Histogram of string
| Date_histogram of string
| Filter of Query.query
| Filters of (string * Query.query) list
| Filters_dynamic
| Top_hits of source_filter option
| Range of string
| Range_keyed of string * string list
| Nested of string
| Reverse_nested

type single = { name : string; agg : agg_type; }
type t = { this : single; sub : t list; }

let analyze_single name agg_type json =
  let field () = U.(get "field" to_string json) in
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
    | "max" | "min" -> Simple_metric (`MinMax, field ())
    | "sum" -> Simple_metric (`Sum, field ())
    | "avg" -> Simple_metric (`Avg, field ())
    | "cardinality" -> Cardinality (field ())
    | "terms" | "significant_terms" -> Terms { field = field (); size = U.member "size" json }
    | "histogram" -> Histogram (field ())
    | "date_histogram" -> Date_histogram (field ())
    | "top_hits" -> Top_hits (Query.extract_source json)
    | "range" when U.(opt "keyed" to_bool json) = Some true ->
      let keys = U.(get "ranges" (to_list (get "key" to_string))) json in
      Range_keyed (field (), keys)
    | "range" -> Range (field ())
    | "nested" -> Nested U.(get "path" to_string json)
    | "reverse_nested" -> Reverse_nested
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
    | Simple_metric (metric, field) ->
      let field_type = (typeof_ mapping field :> resolve_type) in
      let typ =
        match metric with
(*         | `MinMax -> `Maybe (`Typeof field) *)
        | `MinMax -> `Maybe field_type (* TODO use Typeof, but need meta annotation to fallback to field_type *)
        | `Avg -> `Maybe `Double
        | `Sum ->
          match field_type with
          | `Bool -> `Int
          | `Int | `Int64 -> `Dict ["override int as float hack", `Int]
          | _ -> field_type
      in
      [], sub [ "value", typ ]
    | Cardinality _field -> [], sub ["value", `Int ]
    | Terms { field; size } -> (match size with `Var var -> [On_var (var, Eq_type `Int)] | _ -> []), buckets (`Typeof field)
    | Histogram field -> [Field_num field], buckets `Double
    | Date_histogram field -> [Field_date field], buckets `Int ~extra:["key_as_string", `String]
    | Nested _ | Reverse_nested -> [], doc_count ()
    | Filter q -> Query.infer q, doc_count ()
    | Filters l ->  (* TODO other_bucket *)
      let cstrs = l |> List.map snd |> List.map Query.infer |> List.flatten in
      cstrs, keyed_buckets (List.map fst l)
    | Filters_dynamic -> (* by convention assume dynamic filters will be an assoc and so output will be assoc too *)
      [], `Dict [ "buckets", `Object (doc_count ()) ]
    | Top_hits source -> [], `Dict [ "hits", sub ((Hit.hits_ mapping ~highlight:None (*?*) ?nested source) :> (string * resolve_type) list) ]
    | Range field -> [Field_num field], buckets `String
    | Range_keyed (field,keys) -> [Field_num field], keyed_buckets keys
  in
  cstr, (name, shape)

let rec infer mapping ~nested { this; sub } =
  let nested =
    match this.agg with
    | Nested path -> Some (ES_name.make mapping path)
    | _ -> nested (* Reverse_nested ? *)
  in
  let (constraints, subs) = List.split @@ List.map (infer mapping ~nested) sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single mapping ~nested this sub in
  List.flatten (cstr::constraints), desc

let analyze mapping query =
  let (json,sub) = get query in
  json, List.map (infer mapping ~nested:None) sub
