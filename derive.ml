open Prelude

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

let derive mapping query =
  let properties = U.(mapping |> member "properties" |> to_assoc) in
  let aggs = get_aggregations query in
  ()
