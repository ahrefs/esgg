open ExtLib

open Common

type range = { from : bool; to_ : bool }

type agg_type =
| Simple_metric of [`MinMax | `Avg | `Sum ] * value
| Value_count of value
| Cardinality of value
| Terms of { term : value; size : Tjson.t }
| Significant_terms of { term : value; size : Tjson.t }
| Histogram of value
| Date_histogram of { on : value; format : bool }
| Filter of Query.query or_var
| Filters of { filters : [ `Assoc of (string * Query.query or_var) list | `List of Query.query or_var list]; other_bucket : string option; }
| Filters_dynamic of Tjson.var
| Top_hits of { source : source_filter or_var option; highlight : string list option; }
| Range of value
| Range_keyed of value * string list
| Date_range of { on : value; format : bool; keys : string list option; ranges : range list; }
| Nested of string
| Reverse_nested of string option

type single = { name : string; agg : agg_type; }
type t = { this : single; sub : t list; }

let split2 l = List.map (fun (k,x) -> k, fst x) l, List.map (fun (k,x) -> k, snd x) l

let analyze_filter json =
  match json with
  | `Var v -> json, Dynamic v
  | `Assoc _ -> let q = Query.extract_query json in q.json, Static q
  | _ -> fail "filter: expecting either dict or variable"

let analyze_single name agg_type json =
  let value () =
    match U.member "field" json with
    | `String s -> Field s
(*     | `Var v -> Variable v *)
    | `Null ->
      begin match U.assoc "script" json with
      | exception exn -> fail ~exn "failed to get aggregation field"
      | script ->
        try
          match var_or U.to_string script with
          | script -> Script (`Painless, script)
          | exception _ ->
          match U.member "source" script, U.member "id" script with
          | `Null, `Null -> fail "specify script either with \"source\" or \"id\""
          | x, `Null -> Script (`Painless, var_or U.to_string x)
          | `Null, x -> Script (`Id, var_or U.to_string x)
          | _ -> fail "inline and id cannot be used together"
        with exn -> fail ~exn "script"
      end
    | _ -> fail "bad aggregation field"
  in
  let (json, agg) =
    match agg_type with
    | "filter" -> let (json,f) = analyze_filter json in json, Filter f
    | "filters" ->
      let other_bucket =
        match json |> U.member "other_bucket", json |> U.member "other_bucket_key" with
        | `Bool true, `Null -> Some "_other_"
        | (`Null|`Bool true), `String k -> Some k
        | (`Null|`Bool false), _ -> None
        | _ -> fail "weird other_bucket"
      in
      begin match json |> U.member "filters" with
      | `Assoc a ->
        let (jsons,filters) = split2 @@ List.map (fun (k,v) -> k, analyze_filter v) a in
        let json = Tjson.replace json "filters" (`Assoc jsons) in
        json, Filters { filters = `Assoc filters; other_bucket; }
      | `List l->
        let (jsons,filters) = List.split @@ List.map analyze_filter l in
        let json = Tjson.replace json "filters" (`List jsons) in
        json, Filters { filters = `List filters; other_bucket; }
      | `Var v -> json, Filters_dynamic v
      | _ -> fail "filters: expecting either dict or list or variable"
      end
    | _ ->
    json,
    match agg_type with
    | "max" | "min" -> Simple_metric (`MinMax, value ())
    | "sum" -> Simple_metric (`Sum, value ())
    | "avg" -> Simple_metric (`Avg, value ())
    | "value_count" -> Value_count (value ())
    | "cardinality" -> Cardinality (value ())
    | "terms" -> Terms { term = value (); size = U.member "size" json }
    | "significant_terms" -> Significant_terms { term = value (); size = U.member "size" json }
    | "histogram" -> Histogram (value ())
    | "date_histogram" -> Date_histogram { on = value (); format = `Null <> U.member "format" json }
    | "top_hits" -> Top_hits { source = Query.extract_source json; highlight = Query.extract_highlight json; }
    | "range" when U.(opt "keyed" to_bool json) = Some true ->
      let keys = U.(get "ranges" (to_list (get "key" to_string))) json in
      Range_keyed (value (), keys)
    | "range" -> Range (value ())
    | "date_range" ->
      let ranges k = U.(get "ranges" (to_list k)) json in
      let keys =
        match U.(opt "keyed" to_bool json) with
        | Some true -> Some (ranges U.(get "key" to_string))
        | _ -> None
      in
      let ranges = ranges (fun x -> { from = U.mem "from" x; to_ = U.mem "to" x; }) in
      let format = U.mem "format" json in
      Date_range { on = value (); keys; format; ranges }
    | "nested" -> Nested U.(get "path" to_string json)
    | "reverse_nested" -> Reverse_nested U.(opt "path" to_string json)
    | _ -> fail "unknown aggregation type %S" agg_type
  in
  json, { name; agg; }

let extract x =
  let open U in
  let (aggs,rest) = List.partition (function (("aggregations"|"aggs"),_) -> true | _ -> false) (to_assoc x) in
  let aggs =
    match aggs with
    | [] -> []
    | (_,a) :: [] -> to_assoc a
    | _::_::_ -> fail "only one aggregation expected"
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
    | _ -> fail "no aggregation?"
  with
    exn -> fail ~exn "aggregation %S" name

let get x =
  let sub = extract x |> fst |> List.map make in
  `Assoc (List.map (fun (j, agg) -> agg.this.name, j) sub), List.map snd sub

let derive_highlight mapping hl =
  match Hit.of_mapping ~filter:{excludes=None;includes=Some hl} mapping with
  | `Dict l ->
    let l = l |> List.map begin function
    | (k, (`List _ | `List_or_single _ | `Dict _ | `Object _)) -> fail "derive_highlight: expected simple type for %S" k
    | (k, `Maybe t) -> k, `List t (* what will ES do? but seems safe either way *)
    | (k, ((`Ref _ | #simple_type) as t)) -> k, `List t
    end in
    `Dict l
  | _ -> fail "derive_highlight: expected Dict after projecting fields over mapping"

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
          | `Int | `Int64 as t -> `Dict ["override int as float hack", t]
          | _ -> value_type
      in
      [], sub [ "value", typ ]
    | Cardinality _value | Value_count _value -> [], sub ["value", `Int ]
    | Terms { term; size } -> (match size with `Var var -> [On_var (var, Eq_type `Int)] | _ -> []), buckets (`Typeof term)
    | Significant_terms { term; size } ->
      (match size with `Var var -> [On_var (var, Eq_type `Int)] | _ -> []),
(*       buckets ~extra:["score", `Double; "bg_count", `Int] (`Typeof term) *)
      `Dict [ "doc_count", `Int; "bg_count", `Int; "buckets", `List (sub @@ ("key", `Typeof term) :: ("doc_count", `Int) :: ("bg_count", `Int) :: ("score", `Double) ::[]) ]
    | Histogram value -> [Field_num value], buckets `Double
    | Date_histogram { on; format } -> [Field_date on], buckets `Int ~extra:(if format then ["key_as_string", `String] else [])
    | Nested _ | Reverse_nested _ -> [], doc_count ()
    | Filter q -> dynamic_default [] Query.infer q, doc_count ()
    | Filters { filters = `Assoc filters; other_bucket } ->
      let constraints = filters |> List.map snd |> List.map (dynamic_default [] Query.infer) |> List.flatten in
      constraints, keyed_buckets (option_to_list other_bucket @ List.map fst filters)
    | Filters { filters = `List filters; other_bucket=_ } -> (* FIXME other_bucket as last element? *)
      let constraints = filters |> List.map (dynamic_default [] Query.infer) |> List.flatten in
      constraints, `Dict [ "buckets", `List (doc_count ()) ]
    | Filters_dynamic ({ list = false; _ } as v) -> [On_var (v,Eq_object)], `Dict [ "buckets", `Object (doc_count ()) ]
    | Filters_dynamic ({ list = true; _ } as v) -> [On_var (v,Eq_list `Json)], `Dict [ "buckets", `List (doc_count ()) ]
    | Top_hits { source; highlight; } ->
      let highlight = Option.map (derive_highlight mapping) highlight in
      [], `Dict [ "hits", sub ((Hit.hits_ mapping ~highlight ?nested source) :> (string * resolve_type) list) ]
    | Range value -> [Field_num value], buckets `String
    | Range_keyed (value,keys) -> [Field_num value], keyed_buckets keys
    | Date_range { on; format=_; keys; ranges=_ } ->
      [Field_date on], (match keys with None -> buckets `String | Some keys -> keyed_buckets keys)
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
