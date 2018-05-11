open ExtLib

open Common

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

type single = { name : string; agg : agg_type; }
type t = { this : single; sub : t list; }

let analyze_single name agg_type json =
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
      let this = analyze_single name agg_type x in
      let sub = List.map make sub in
      { this; sub }
    | _ -> Exn.fail "no aggregation?"
  with
    exn -> Exn.fail ~exn "aggregation %S" name

let get x =
  extract x |> fst |> List.map make

let infer_single { name; agg; } sub =
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

let rec infer { this; sub } =
  let (constraints, subs) = List.split @@ List.map infer sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single this sub in
  List.flatten (cstr::constraints), desc

let analyze query = List.map infer (get query)
