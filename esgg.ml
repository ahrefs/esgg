module Json = Yojson.Safe

let () =
  let json fname = Json.from_file ~fname fname in
  let template file = try Tjson.parse @@ Std.input_file file with exn -> Exn.fail ~exn "load %S" file in
  match Action.args with
  | ["derive";mapping;query] -> Derive.derive (json mapping) (template query)
  | ["derive";name;mapping;query] -> Derive.derive ~name (json mapping) (template query)
  | ["query";mapping;query] -> Derive.query (json mapping) (template query)
  | ["query";name;mapping;query] -> Derive.query ~name (json mapping) (template query)
  | ["reflect";name;mapping] -> Derive.reflect name (json mapping)
  | ["tjson";file] -> Tjson.lift (fun x -> x) (template file)
  | ["parse_json";file] -> Tjson.parse_json (Std.input_file file)
  | _ -> assert false
