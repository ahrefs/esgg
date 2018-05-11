open Printf
open Prelude
open Common

let loc = Atd_ast.dummy_loc
let annot section l = (section, (loc, List.map (fun (k,v) -> k, (loc, Some v)) l))
let annots a = Atd_annot.merge @@ List.map (fun (s,l) -> annot s l) a
let list ?(a=[]) t = `List (loc,t,annots a)
let tuple l = `Tuple (loc, List.map (fun t -> (loc,t,[])) l, [])
let record fields = `Record (loc,fields,[])
let field ?(kind=`Required) ?(a=[]) n t = `Field (loc, (n, kind, annots a), t)
let pname ?(a=[]) t params = `Name (loc,(loc,t,params),annots a)
let tname ?a t = pname ?a t []
let nullable ?(a=[]) t = `Nullable (loc,t,annots a)
let option ?(a=[]) t = `Option (loc,t,annots a)
let wrap ocaml t = `Wrap (loc,t,annots ["ocaml",ocaml])
let tvar t = `Tvar (loc,t)
let ptyp ?(a=[]) name params t = `Type (loc, (name,params,annots a), t)
let typ ?a name t = ptyp ?a name [] t

let of_simple_type =
  function
  | `Int -> tname "int"
  | `Int64 -> tname ~a:["ocaml",["repr","int64"]] "int"
  | `String -> tname "string"
  | `Double -> tname "float"
  | `Bool -> tname "bool"

let of_nullable_type = function
| #simple_type as t -> of_simple_type t
| `Maybe t -> nullable @@ of_simple_type t

let wrap_ref ref t = wrap ["module",ES_name.to_ocaml ref] (of_simple_type t)

let of_var_type' : var_type' -> Atd_ast.type_expr = function
| #simple_type as t -> of_simple_type t
| `Ref (ref,t) -> wrap_ref ref t
| `List (`Ref (ref,t)) -> list (wrap_ref ref t)
| `List (#simple_type as t) -> list (of_simple_type t)
| `Json -> tname "basic_json"

let of_var_type : var_type -> Atd_ast.type_expr = function
| #var_type' as t -> of_var_type' t
| `Optional t -> option @@ of_var_type' t

let safe_name name =
  match name with
  | "type" -> ["json",["name",name]],"type_"
  | s -> [],s

let of_shape name (shape:result_type) =
  let names = Hashtbl.create 10 in
  let types = ref [] in
  let fresh_name t =
    let (prefix,start) =
      match t with
      | `Record (_,[`Field (_,(name,_,_),_)],_) -> name, name
      | _ -> "t", "t0"
    in
    let rec loop name n =
      let (_,name) (* TODO *) = safe_name name in
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
  let rec map ?push shape = snd @@ map' ?push shape
  and map' ?(push=ref_name) shape =
    match shape with
    | #nullable_type as c -> [], of_nullable_type c
    | `Ref (ref,t) -> ["doc",["text",ES_name.show ref]], wrap_ref ref t
    | `List t -> [], list (map t)
    | `Assoc (k,v) -> [], list ~a:["json",["repr","object"]] (tuple [map k; map v])
    | `Dict ["key",k; "doc_count", `Int] -> [], pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> [], pname "buckets" [map t]
    | `Dict fields ->
      let fields = fields |> List.map begin fun (name,t) ->
        let kind = match t with `Maybe _ -> `Optional | `List _ -> `With_default | _ -> `Required in
        let (a,t) = map' t in
        let (a',name) = safe_name name in
        let a = a' @ a in
        field ~a ~kind name t
      end in
      [], push @@ record fields
  in
  tuck types (typ name (map ~push:id shape));
  (loc,[]), List.rev !types

let of_vars l =
  let basic_json =
    if List.exists (fun (_,t) -> t = `Json) l then
      [typ "basic_json" ~a:["ocaml",["module","Json";"t","json"]] (tname "abstract")]
    else
      []
  in
  let input = [typ "input" (record (List.map (fun (n,t) -> field n (of_var_type t)) l))] in
  (loc,[]), (basic_json @ input)
