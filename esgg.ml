open Prelude

module Json = Yojson.Basic

let print_atd x = print_endline @@ Easy_format.Pretty.to_string @@ Atd_print.format x

let tjson tjson = print_endline @@ Tjson.lift (fun x -> x) tjson

let input mapping query =
  let (vars,map) = Query.analyze mapping query in
  let show_var_type = function
  | `Json -> "json"
  | #Common.simple_type as t -> Common.show_simple_type t
  in
  if vars <> [] then printfn "(*";
  vars |> List.iter (fun (name,typ) -> printfn "%s : %s" name (show_var_type typ));
  if vars <> [] then printfn "*)";
  print_string "let make = ";
  print_endline @@ Tjson.lift map query

let output mapping query = print_atd @@ Derive.output mapping query

let () =
  let json fname = Json.from_file ~fname fname in
  let template file = try Tjson.parse @@ Std.input_file file with exn -> Exn.fail ~exn "load %S" file in
  let map_query f l =
    let f ?name mapping query = f { Common.mapping = json mapping; name } (template query) in
    match l with
    | [m;q] -> f m q
    | [name;m;q] -> f ~name m q
    | _ -> Exn.fail "expected <mapping.json> and <query.template.json>"
  in
  match Action.args with
  | "output"::tl -> map_query output tl
  | "input"::tl -> map_query input tl
  | ["reflect";name;mapping] -> Derive.print_reflect name (json mapping)
  | ["tjson";file] -> tjson (template file)
  | ["parse_json";file] -> Tjson.print_parse_json (Std.input_file file)
  | _ -> assert false
