open Prelude

module Json = Yojson.Basic

let print_atd x = print_endline @@ Easy_format.Pretty.to_string @@ Atd_print.format x

let tjson tjson = print_endline @@ Tjson.lift tjson

let input_direct mapping json =
  let (vars,map,json) = Query.analyze mapping json in
  if vars <> [] then printfn "(*";
  vars |> List.iter (fun (name,typ) -> printfn "%s : %s" name (Common.show_var_type typ));
  if vars <> [] then printfn "*)";
  print_string "let make = ";
  print_endline @@ Tjson.lift_ map json

let show_as_record_fields vars =
  Printf.sprintf "{%s}" @@ String.concat "; " @@ List.map (fun (name,_typ) -> name) vars

let input_j mapping json =
  let (vars,map,json) = Query.analyze mapping json in
  printfn "let make %s =" (if vars = [] then "()" else show_as_record_fields vars);
  printfn "  %s" (Tjson.lift_to_string map json)

let vars mapping query =
  let (vars,_,_) = Query.analyze mapping query in
  print_atd @@ Derive.atd_of_vars vars

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
  | "input_direct"::tl -> map_query input_direct tl
  | "input_j"::tl -> map_query input_j tl
  | "vars"::tl -> map_query vars tl
  | ["reflect";name;mapping] -> Derive.print_reflect name (json mapping)
  | ["tjson";file] -> tjson (template file)
  | ["parse_json";file] -> Tjson.print_parse_json (Std.input_file file)
  | _ -> assert false
