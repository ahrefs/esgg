open Printf
open Devkit

module Json = Yojson.Basic

let print_atd x = print_endline @@ Easy_format.Pretty.to_string @@ Atd.Print.format x

let tjson tjson = print_endline @@ Tjson.lift tjson

(*
let input_direct mapping json =
  let (vars,map,json) = Derive.derive mapping json in
  if vars <> [] then printfn "(*";
  vars |> List.iter (fun (name,(req,_typ)) -> printfn "%s : %s" name (Common.show_required req));
  if vars <> [] then printfn "*)";
  print_string "let make = ";
  print_endline @@ Tjson.lift_ map json
*)

let show_as_record_fields vars =
  sprintf "{%s}" @@ String.concat "; " @@ List.map (fun (name,_typ) -> name) vars

let input_j ~header _init mapping json =
  let (query,vars,map,(action,url,args,body)) = Derive.derive mapping json in
  printfn "%s" header;
  printfn "let type_ = %s" (match query with Search _ -> "`Search" | Get _ -> "`Get" | Mget _ -> "`Mget");
  printfn "let make ~index:(__esgg_index,__esgg_kind) %s =" (if vars = [] then "()" else show_as_record_fields vars);
  printfn "  %s," action;
  printfn "  %s," url;
  printfn "  %s," args;
  match body with
  | Some json -> printfn "  Some (%s)" (Tjson.lift_to_string map json)
  | None -> printfn "  None"

let vars ~header init mapping query =
  let (_,vars,_,_) = Derive.derive mapping query in
  let atd = Atdgen.of_vars ~init vars in
  print_endline header;
  print_atd atd

let output ~header init mapping query =
  let atd = Derive.output ~init mapping query in
  print_endline header;
  print_atd atd

let help () = [
  "ElasticSearch Guided (code) Generator";
  "";
  "Supported commands are :";
  "  input_j [-name <name>] [-shared <file.atd>] <mapping.json> <query.json> # generate OCaml function to invoke query with substitutions";
  "  vars [-name <name>] [-shared <file.atd>] <mapping.json> <query.json> # derive atd input type for the ES query";
  "  output [-name <name>] [-shared <file.atd>] <mapping.json> <query.json> # derive output atd of ES query";
  "  reflect <name> <mapping.json> # reflect mapping into OCaml module";
  "  tjson <file.json> # lift json template to code";
  "  parse_json <file.json> # debug json template parsing";
] |> List.iter print_endline

let () =
  let json fname = Json.from_file ~fname fname in
  let template file = try Tjson.parse @@ Std.input_file file with exn -> Exn.fail ~exn "load %S" file in
  let map_query f l =
    let init = ref None in
    let name = ref None in
    let f mapping query =
      let init = match !init with None -> ((Atd.Ast.dummy_loc,[]),[]) | Some fname -> Atdgen.parse_file fname in
      let header = sprintf "(* Generated by esgg from %s based on %s *)" query mapping in
      f ~header init { Common.mapping = json mapping; name = !name } (template query)
    in
    let rec loop = function
    | "-shared"::s::tl -> init := Some s; loop tl
    | "-name"::s::tl -> name := Some s; loop tl
    | [m;q] -> f m q
    | [s;m;q] -> name := Some s; f m q
    | _ -> Exn.fail "expected: [-shared <shared.atd>] [-name <mapping>] <mapping.json> <query.template.json>"
    in
    loop l
  in
  match Action.args with
  | "output"::tl -> map_query output tl
(*   | "input_direct"::tl -> map_query input_direct tl *)
  | "input_j"::tl -> map_query input_j tl
  | "vars"::tl -> map_query vars tl
  | ["reflect";name;mapping] -> Derive.print_reflect name (json mapping)
  | ["tjson";file] -> tjson (template file)
  | ["parse_json";file] -> Tjson.print_parse_json (Std.input_file file)
  | [] | ["-h"|"-help"|"--help"] -> help ()
  | _ ->
    prerr_endline "Didn't get that! Try `esgg -h`";
    exit 2
