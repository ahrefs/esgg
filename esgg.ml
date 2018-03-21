module Json = Yojson.Safe

let () =
  match Action.args with
  | ["derive";mapping;query] -> Derive.derive (Json.from_file ~fname:mapping mapping) (Json.from_file ~fname:query query)
  | ["derive";name;mapping;query] -> Derive.derive ~name (Json.from_file ~fname:mapping mapping) (Json.from_file ~fname:query query)
  | _ -> assert false
