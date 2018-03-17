module Json = Yojson.Safe

let () =
  match Action.args with
  | ["derive";mapping;query] -> Derive.derive (Json.from_file mapping) (Json.from_file query)
  | _ -> assert false
