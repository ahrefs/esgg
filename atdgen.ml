open Printf
open ExtLib
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

let wrap_ref ref t : Atd_ast.type_expr = wrap ["module",ES_name.to_ocaml ref] t

let of_var_type {multi;ref;typ} : Atd_ast.type_expr =
  let t = of_simple_type typ in
  let t = match ref with Some es_name -> wrap_ref es_name t | None -> t in
  match multi with
  | One -> t
  | Many -> list t

let safe_ident name =
  let safe =
    match name with
    | "type" -> Some "type_"
    | s ->
      let s' = to_valid_ident ~prefix:"t_" s in
      if s = s' then None else Some s'
  in
  match safe with
  | Some s -> ["json",["name",name]], s
  | None -> [], name

module New_types() : sig

open Atd_ast

val new_ : module_item -> unit
val ref_ : ?name:string -> type_expr -> type_expr
val get : unit -> module_item list

end = struct

  let names = Hashtbl.create 10
  let types = ref []

  let fresh_name ?name t =
    let candidates =
      (match name with Some name -> [ name, name ] | None -> [])
      @
      match t with
      | `Record (_,[`Field (_,(name,_,_),_)],_) -> [ name, name ]
      | _ -> []
    in
    let candidates = if candidates = [] then [ "t", "t0" ] else candidates in
    let check name =
      let (_,name) (* TODO *) = safe_ident name in
      match Hashtbl.mem names name with
      | true -> None
      | false -> Some name
    in
    let rec loop acc candidates n =
      match candidates with
      | [] -> loop [] (List.rev acc) (n+1)
      | (prefix,name)::xs ->
        match check name with
        | None -> loop ((prefix,sprintf "%s%d" prefix n)::acc) xs n
        | Some name -> name
    in
    loop [] candidates 1

  let new_ typ =
    tuck types typ;
    let `Type (_,(name,_,_),_) = typ in
    assert (not @@ Hashtbl.mem names name);
    Hashtbl.add names name ()

  (* atdgen does topological sorting itself, but still lets try to keep natural order *)
  let replace name typ =
    match List.partition (function`Type (_,(n,_,_),_) -> n = name) !types with
    | [`Type (loc,(n,tp,annot),_)], rest -> types := (`Type (loc,(n,tp,annot),typ)) :: rest
    | _ -> assert false

  let ref_ ?name t =
    match t with
    | `Record _ ->
      begin match List.find (fun (`Type (_,_,v)) -> t = v) !types with
      | `Type (_,(name,[],_),_) -> tname name
      | _ -> assert false (* parametric type cannot match *)
      | exception _ ->
        let name = fresh_name ?name t in
        new_ @@ typ name t;
        tname name
      end
    | _ -> t

  (* remove nested records, producing more types as needed *)
  let unrecord name t =
    let rec map name t =
      match t with
      | `Record r -> ref_ ?name (map_record r)
      | `Sum (_loc, _variants, _annot) -> assert false
      | `Tuple (loc, cells, annot) -> `Tuple (loc, List.map (fun (loc,t,annot) -> loc, map None t, annot) cells, annot)
      | `List (loc, t, annot) ->
        let name =
          match name with
          | Some name when String.ends_with name "s" && String.length name > 1 -> Some (String.slice ~last:(-1) name)
          | Some name -> Some (name ^ "_elem")
          | None -> None
        in
        `List (loc, map name t, annot)
      | `Option (loc, t, annot) -> `Option (loc, map name t, annot)
      | `Nullable (loc, t, annot) -> `Nullable (loc, map name t, annot)
      | `Shared (loc, t, annot) -> `Shared (loc, map name t, annot)
      | `Wrap (loc, t, annot) -> `Wrap (loc, map name t, annot)
      | `Name (loc, (loc', n, tl), annot) ->
        let name = match n, tl with "buckets", [_] -> name | _ -> None in
        `Name (loc, (loc', n, List.map (map name) tl), annot)
      | `Tvar _ -> t
    and map_record (loc,fields,annot) =
      let fields = fields |> List.map begin function
      | `Inherit _ -> Exn.fail "inherit not supported"
      | `Field (loc,(name,_,_ as f),t) -> `Field (loc, f, map (Some name) t)
      end
      in
      `Record (loc,fields,annot)
    in
    match t with
    | `Record r -> map_record r (* record type top top-level, no need to unnest *)
    | _ -> map (Some name) t

  let get () =
    !types |> List.rev |> List.iter (function `Type (_,(name,_,_),t) -> replace name (unrecord name t));
    List.rev !types

end

let of_shape name (shape:result_type) : Atd_ast.full_module =
  let module Types = New_types() in
  Types.new_ @@ ptyp "doc_count" ["key"] (record [field "key" (tvar "key"); field "doc_count" (tname "int")]);
  Types.new_ @@ ptyp "buckets" ["a"] (record [field "buckets" (list (tvar "a"))]);
  let rec map shape =
    match shape with
    | #simple_type as t -> of_simple_type t
    | `Maybe t -> nullable @@ map t
    | `Ref (ref,t) -> wrap_ref ref (of_simple_type t)
    | `List t -> list (map t)
    | `Assoc (k,v) -> list ~a:["json",["repr","object"]] (tuple [map k; map v])
    | `Dict ["key",k; "doc_count", `Int] -> pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> pname "buckets" [map t]
    | `Dict fields ->
      let fields = fields |> List.map begin fun (name,t) ->
        let kind = match t with `Maybe _ -> `Optional | `List _ -> `With_default | _ -> `Required in
        let t = map t in
        let (a,name) = safe_ident name in (* TODO check unique *)
        field ~a ~kind name t
      end in
      record fields
  in
  Types.new_ @@ typ name (map shape);
  (loc,[]), Types.get ()

let of_vars (l:input_vars) =
  let module Types = New_types() in
  let basic_json = lazy (Types.new_ @@ typ "basic_json" ~a:["ocaml",["module","Json";"t","json"]] (tname "abstract")) in
  let rec map_field (req,t) : Atd_ast.type_expr =
    let t =
      match t with
      | `Group l -> Types.ref_ @@ map l
      | `Simple t ->
        match t with
        | None -> Lazy.force basic_json; tname "basic_json"
        | Some t -> of_var_type t
    in
    match req with
    | `Required -> t
    | `Optional -> option t
  and map l =
    match l with
    | [] -> tname "unit"
    | _ -> record (List.map (fun (n,t) -> field n (map_field t)) l)
  in
  Types.new_ @@ typ "input" (map l);
  (loc,[]), (Types.get ())
