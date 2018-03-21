open ExtLib
open Prelude
open Printf

module Json = Yojson.Safe
module U = Json.Util

type mapping = { mapping : Json.json; name : string option; }

type simple_type = [ `Int | `Int64 | `String | `Double ]

type result_type = [
  | `List of result_type
  | `Dict of (string * result_type) list
  | `Assoc of (result_type * result_type)
  | `Ref of (string list * simple_type) (* reference field in mapping *)
  | simple_type
  ]

type agg_type =
| Simple_metric of string
| Cardinality of string
| Terms of string
| Histogram of string
| Date_histogram of string
| Filter
| Filters
| Top_hits
| Range of string

type single_aggregation = { name : string; agg : agg_type; }
type aggregation = { this : single_aggregation; sub : aggregation list; }

let get json k conv = try U.member k json |> conv with exn -> Exn.fail ~exn "get %S" k

let analyze_single_aggregation name agg_type json =
  let field () = get json "field" U.to_string in
  let agg =
    match agg_type with
    | "max" | "min" | "avg" | "sum" -> Simple_metric (field ())
    | "cardinality" -> Cardinality (field ())
    | "terms" | "significant_terms" -> Terms (field ())
    | "histogram" -> Histogram (field ())
    | "date_histogram" -> Date_histogram (field ())
    | "filter" -> Filter
    | "filters" -> Filters
    | "top_hits" -> Top_hits
    | "range" -> Range (field ()) (* TODO keyed *)
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
    exn -> Exn.fail ~exn "while analyzing aggregation %S" name

let get_aggregations x =
  extract_aggregations x |> fst |> List.map aggregation

let infer_single_aggregation { name; agg; } sub =
  let buckets t = `Dict [ "buckets", `List (sub ["key", t; "doc_count", `Int]) ] in
  let (cstr,shape) =
    match agg with
    | Simple_metric field -> [`Is_num field], sub [ "value", `Double ]
    | Cardinality _field -> [], sub ["value", `Int ]
    | Terms field -> [], buckets (`Typeof field)
    | Histogram field -> [`Is_num field], buckets `Double
    | Date_histogram field -> [`Is_date field], buckets `Int
    | Filter -> [], sub ["doc_count", `Int]
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

let atd_of_es_type = function
| "long" -> `Int
| "keyword" | "text" -> `String
| s -> Exn.fail "atd_of_es_type: cannot handle %S" s

let mod_of_es_name s = List.map String.capitalize s |> String.concat "."

let resolve_types mapping shape : result_type =
  let typeof t =
    let rec find path schema =
      match path with
      | [] -> get schema "type" U.to_string
      | hd::tl -> find tl (List.assoc hd (get schema "properties" U.to_assoc))
    in
    match find t mapping.mapping with
    | exception _ -> Exn.fail "no such field"
    | "long" when List.exists (fun s -> String.exists s "hash") t -> `Int64 (* hack *)
    | a -> atd_of_es_type a
  in
  let typeof x = try typeof x with exn -> Exn.fail ~exn "failed to type field %S" (String.concat "." x) in
  let ref path =
    match mapping.name with
    | None -> path
    | Some base -> base :: path
  in
  let rec map = function
  | `List t -> `List (map t)
  | `Dict fields -> `Dict (List.map (fun (n,t) -> n, map t) fields)
  | `Assoc (k,v) -> `Assoc (map k, map v)
  | `Typeof x -> let path = Stre.nsplitc x '.' in `Ref (ref path, typeof path)
  | `Int64 | `Int | `String | `Double as t -> t
  in
  map shape

let newname =
  let nr = ref 0 in
  fun () -> incr nr; sprintf "t%d" !nr

module Gen = struct

  open Atd_ast
  let loc = dummy_loc
  let annot section l = (section, (loc, List.map (fun (k,v) -> k, (loc, Some v)) l))
  let annots = List.map (fun (s,l) -> annot s l)
  let list ?(a=[]) t = `List (loc,t,annots a)
  let tuple l = `Tuple (loc, List.map (fun t -> (loc,t,[])) l, [])
  let record fields = `Record (loc,fields,[])
  let field ?(a=[]) n t = `Field (loc, (n, `Required, annots a), t)
  let pname ?(a=[]) t params = `Name (loc,(loc,t,params),annots a)
  let tname ?a t = pname ?a t []
  let wrap ocaml t = `Wrap (loc,t,annots ["ocaml",ocaml])
  let tvar t = `Tvar (loc,t)
  let ptyp name params t = `Type (loc, (name,params,[]), t)
  let typ name t = ptyp name [] t

end

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
  let map_simple_type = function
    | `Int -> tname "int"
    | `Int64 -> tname ~a:["ocaml",["repr","int64"]] "int"
    | `String -> tname "string"
    | `Double -> tname "float"
  in
  let rec map ?push shape = snd @@ map' ?push shape
  and map' ?(push=ref_name) shape =
    match shape with
    | #simple_type as c -> [], map_simple_type c
    | `Ref (ref,t) -> ["doc",["text",String.concat "." ref]], wrap ["module",mod_of_es_name ref] (map_simple_type t)
    | `List t -> [], list (map t)
    | `Dict ["key",k; "doc_count", `Int] -> [], pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> [], pname "buckets" [map t]
    | `Dict fields -> [], push @@ record (List.map (fun (n,t) -> let (a,t) = map' t in field ~a n t) fields)
    | `Assoc (k,v) -> [], list ~a:["json",["repr","object"]] (tuple [map k; map v])
  in
  tuck types (typ name (map ~push:id shape));
  (loc,[]), List.rev !types

let derive_atd shape =
  print_endline @@ Easy_format.Pretty.to_string @@ Atd_print.format @@ atd_of_shape "result" shape

let derive ?name mapping query =
  let mapping = { mapping; name } in
  let aggs = List.map snd @@ analyze_aggregations query in (* XXX discarding constraints *)
  let result = `Dict (("hits", `Dict ["total", `Int]) :: (if aggs = [] then [] else ["aggregations", `Dict aggs])) in
  derive_atd @@ resolve_types mapping result;
  ()

let uident s =
  assert (s <> "");
  let s = String.map (function ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c) -> c | _ -> '_') s in
  let s = match s.[0] with '0'..'9' -> "M_" ^ s | _ -> s in
  String.capitalize s

let reflect name mapping =
  let rec iter hash nr_indent (name,x) =
    let indent = String.make nr_indent ' ' in
    let hash = hash || String.exists name "hash" in
    match U.member "type" x, U.member "properties" x with
    | `String typ, `Null ->
      let typ = if typ = "long" && hash then "int64" else typ in
      printfn "%smodule %s = Id(%s)" indent (uident name) (uident typ)
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
