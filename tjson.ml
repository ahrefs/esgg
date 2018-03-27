open Printf
open ExtLib

let pp_string f x =
  let b = Buffer.create 10 in
  let pf = Format.formatter_of_buffer b in
  f pf x;
  Format.pp_flush_formatter pf;
  Buffer.contents b

let sub_decoded d s =
  let ((l1,c1),(l2,c2)) = Jsonm.decoded_range d in
  let find_line l =
    let rec loop i = function
    | 0 -> assert false
    | 1 -> i
    | l -> loop (String.index_from s i '\n' + 1) (l - 1)
    in
    loop 0 l
  in
  let first = find_line l1 + c1 - 1 in
  let last = find_line l2 + c2 in
  String.slice ~first ~last s

let var_name s =
  match Scanf.sscanf s "$%_[a-zA-Z]%_[0-9_a-zA-Z]%!" () with
  | exception _ -> Exn.fail "bad var name %S" s
  | () -> String.slice ~first:1 s

let parse s =
  let exception Escape of ((int * int) * (int * int)) * Jsonm.error in
  let lexeme d =
    match Jsonm.decode d with
    | `Lexeme l -> (l :> [Jsonm.lexeme|`Var of string])
    | `Error (`Expected `Value) -> `Var (var_name @@ sub_decoded d s)
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d =
    match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null | `Bool _ | `String _ | `Float _ | `Var _ as v -> k v d
    | _ -> assert false
  and arr vs k d =
    match lexeme d with
    | `Ae -> k (`List (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d =
    match lexeme d with
    | `Oe -> k (`Assoc (List.rev ms)) d
    | `Name n -> value (lexeme d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder @@ `String s in
  try
    let v = value (lexeme d) (fun v _ -> v) d in
    match Jsonm.decode d with
    | `End -> v
    | `Lexeme l -> Exn.fail "expected End, got %s" (pp_string Jsonm.pp_lexeme l)
    | `Error e -> Exn.fail "expected End, got %s" (pp_string Jsonm.pp_error e)
    | `Await -> assert false
  with
    Escape (((l1,c1),(l2,c2)),e) -> Exn.fail "E: %d,%d-%d,%d %s" l1 c1 l2 c2 (pp_string Jsonm.pp_error e)

let lift_to_string v =
  let module J = Yojson.Basic in
  let module Bi = Bi_outbuf in
  let out = Buffer.create 10 in
  let cur = Bi.create 10 in
  let rec write = function
  | `Null -> J.write_null cur ()
  | `Bool b -> J.write_bool cur b
  | `String s -> J.write_string cur s
  | `Float f -> J.write_float cur f
  | `Var name -> bprintf out "%S^%s^" (Bi.contents cur) name; Bi.reset cur
  | `List l -> Bi.add_char cur '['; List.iter write l; Bi.add_char cur ']'
  | `Assoc a ->
    Bi.add_char cur '{';
    List.iter (fun (k,v) -> J.write_string cur k; Bi.add_char cur ':'; write v) a;
    Bi.add_char cur '}'
  in
  write v;
  Buffer.add_string out (Bi.contents cur);
  Buffer.contents out

let rec fold f acc = function
| `Null | `Bool _ | `String _ | `Float _ | `Var _ as x -> f acc x
| `List l -> List.fold_left (fold f) acc l
| `Assoc a -> List.fold_left (fun acc (_,v) -> fold f acc v) acc a

let vars v =
  List.unique ~cmp:String.equal (fold (fun acc x -> match x with `Var name -> name::acc | _ -> acc) [] v)

let parse_show s =
  let v = parse s in
  printf "fun ";
  List.iter (fun var -> printf "~%s " var) (vars v);
  printf "() -> %s" (lift_to_string v);
  print_newline ();
  ()
