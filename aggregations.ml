open ExtLib

open Common

type agg_type =
| Simple_metric of string
| Cardinality of string
| Terms of { field : string; size : Tjson.t }
| Histogram of string
| Date_histogram of string
| Filter of Query.t
| Filters of (string * Query.t) list
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
    | "filter" -> Filter (Query.extract_query json)
    | "filters" -> Filters (json |> U.member "filters" |> U.to_assoc |> List.map (fun (k,v) -> k, Query.extract_query v))
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
  let doc_count () = sub ["doc_count", `Int] in
  let (cstr,shape) =
    match agg with
    | Simple_metric field -> [Query.Field_num field], sub [ "value", `Maybe `Double ]
    | Cardinality _field -> [], sub ["value", `Int ]
    | Terms { field; size } -> (match size with `Var var -> [On_var (var, Eq_type `Int)] | _ -> []), buckets (`Typeof field)
    | Histogram field -> [Field_num field], buckets `Double
    | Date_histogram field -> [Field_date field], buckets `Int ~extra:["key_as_string", `String]
    | Nested _ | Reverse_nested -> [], doc_count ()
    | Filter q -> Query.infer q, doc_count ()
    | Filters l ->  (* TODO other_bucket *)
      let cstrs = l |> List.map (fun (_,q) -> Query.infer q) |> List.flatten in
      cstrs, `Dict [ "buckets", `Assoc (`String, doc_count ())]
    | Top_hits -> [], `Dict [ "hits", `Dict [ "total", `Int ] ]
    | Range field -> [Field_num field], `Dict [ "buckets", `List (doc_count ()) ]
  in
  cstr, (name, shape)

let rec infer { this; sub } =
  let (constraints, subs) = List.split @@ List.map infer sub in
  let sub l = `Dict (l @ subs) in
  let (cstr,desc) = infer_single this sub in
  List.flatten (cstr::constraints), desc

let analyze query = List.map infer (get query)
