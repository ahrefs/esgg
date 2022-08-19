open Printf
open ExtLib
open Common

let loc = Atd.Ast.dummy_loc
let annot section l = (section, (loc, List.map (fun (k,v) -> k, (loc, Some v)) l))
let annots a = Atd.Annot.merge @@ List.map (fun (s,l) -> annot s l) a
let list ?(a=[]) t = Atd.Ast.List (loc,t,annots a)
let tuple l = Atd.Ast.Tuple (loc, List.map (fun t -> (loc,t,[])) l, [])
let record fields = Atd.Ast.Record (loc,fields,[])
let field ?(kind=Atd.Ast.Required) ?(a=[]) n t = `Field (loc, (n, kind, annots a), t)
let pname ?(a=[]) t params = Atd.Ast.Name (loc,(loc,t,params),annots a)
let tname ?a t = pname ?a t []
let nullable ?(a=[]) t = Atd.Ast.Nullable (loc,t,annots a)
let wrap t ocaml = Atd.Ast.Wrap (loc,t,annots ["ocaml",ocaml])
let tvar t = Atd.Ast.Tvar (loc,t)
let ptyp ?(a=[]) name params t = Atd.Ast.Type (loc, (name,params,annots a), t)
let typ ?a name t = ptyp ?a name [] t

let of_simple_type =
  function
  | Int -> tname "int"
  | Int64 -> tname ~a:["ocaml",["repr","int64"]] "int"
  | String | Date -> tname "string"
  | Double -> tname "float"
  | Bool -> tname "bool"
  | Json -> tname "basic_json"

let wrap_ref ref t : Atd.Ast.type_expr = wrap t ["module",ES_name.to_ocaml ref]

let of_var_type {cardinality;ref;typ} : Atd.Ast.type_expr =
  let t = of_simple_type typ in
  let t = match ref with Some es_name -> wrap_ref es_name t | None -> t in
  match cardinality with
  | One -> t
  | Many -> list t

(* http://caml.inria.fr/pub/docs/manual-ocaml-4.07/manual049.html *)
let ocaml_keywords = [
"and";
"as";
"asr";
"assert";
"begin";
"class";
"constraint";
"do";
"done";
"downto";
"else";
"end";
"exception";
"external";
"false";
"for";
"fun";
"function";
"functor";
"if";
"in";
"include";
"inherit";
"initializer";
"land";
"lazy";
"let";
"lor";
"lsl";
"lsr";
"lxor";
"match";
"method";
"mod";
"module";
"mutable";
"new";
"nonrec";
"object";
"of";
"open!";
"open";
"or";
"private";
"rec";
"sig";
"struct";
"then";
"to";
"true";
"try";
"type";
"val";
"virtual";
"when";
"while";
"with";
]

let safe_ident name =
  let safe =
    if List.mem name ocaml_keywords then
      Some (name ^ "_")
    else
      let s' = to_valid_ident ~prefix:"t_" name in
      if name = s' then None else Some s'
  in
  match safe with
  | Some s -> ["json",["name",name]], s
  | None -> [], name

(* diligent copy of Ast.amap_type_expr *)
let rec reloc_type_expr f (x : Atd.Ast.type_expr) =
  match x with
      Atd.Ast.Sum (loc, vl, a) -> Atd.Ast.Sum (f loc, List.map (reloc_variant f) vl, reloc_annot f a)
    | Record (loc, fl, a) -> Record (f loc, List.map (reloc_field f) fl, reloc_annot f a)
    | Tuple (loc, tl, a) -> Tuple (f loc, List.map (reloc_cell f) tl, reloc_annot f a)
    | List (loc, t, a) -> List (f loc, reloc_type_expr f t, reloc_annot f a)
    | Option (loc, t, a) -> Option (f loc, reloc_type_expr f t, reloc_annot f a)
    | Nullable (loc, t, a) -> Nullable (f loc, reloc_type_expr f t, reloc_annot f a)
    | Shared (loc, t, a) -> Shared (f loc, reloc_type_expr f t, reloc_annot f a)
    | Wrap (loc, t, a) -> Wrap (f loc, reloc_type_expr f t, reloc_annot f a)
    | Tvar (loc, s) -> Tvar (f loc, s)
    | Name (loc, (loc2, name, args), a) ->
        Name (f loc, (f loc2, name, List.map (reloc_type_expr f) args), reloc_annot f a)

and reloc_variant f = function
    Atd.Ast.Variant (loc, (name, a), o) ->
      let o =
        match o with
            None -> None
          | Some x -> Some (reloc_type_expr f x)
      in
      Variant (f loc, (name, reloc_annot f a), o)
  | Inherit (loc, x) ->
      Inherit (f loc, reloc_type_expr f x)

and reloc_field f = function
    `Field (loc, (name, kind, a), x) ->
      `Field (f loc, (name, kind, reloc_annot f a), reloc_type_expr f x)
  | `Inherit (loc, x) ->
      `Inherit (f loc, reloc_type_expr f x)

and reloc_cell f (loc, x, a) =
  (f loc, reloc_type_expr f x, reloc_annot f a)

and reloc_annot f = List.map (fun (s,(loc,a)) -> (s,(f loc,List.map (fun (s,(loc,a)) -> (s,(f loc,a))) a)))

let type_def (Atd.Ast.Type (_,(name,_,_),ty)) = name, ty
let type_def_name t = fst @@ type_def t
let show_type_def ty = Easy_format.Pretty.to_string @@ Atd.Print.format ((loc,[]),[ty])

module Types : sig

open Atd.Ast

type t

val empty : unit -> t
val add : t -> module_item -> unit
val get : t -> module_item list

end = struct

  type t = { names : (string,unit) Hashtbl.t; types : Atd.Ast.module_item list ref; }

  let empty () = { names = Hashtbl.create 10; types = ref []; }

  let reloc_dummy = reloc_type_expr (fun _loc -> Atd.Ast.dummy_loc)

  let fresh_name t ?name ty =
    let candidates =
      (match name with Some name -> [ name, name ] | None -> [])
      @
      match ty with
      | Atd.Ast.Record (_,[`Field (_,(name,_,_),_)],_) -> [ name, name ]
      | _ -> []
    in
    let candidates = if candidates = [] then [ "t", "t0" ] else candidates in
    let check name =
      let (_,name) (* TODO *) = safe_ident name in
      match Hashtbl.mem t.names name with
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

  let new_ t ty =
    tuck t.types ty;
    let name = type_def_name ty in
    assert (not @@ Hashtbl.mem t.names name);
    Hashtbl.add t.names name ()

  let add t td =
    let (name,ty) = type_def td in
    match Hashtbl.find t.names name with
    | exception _ -> new_ t td
    | () ->
    match List.find (fun ty' -> type_def_name ty' = name) !(t.types) with
    | exception _ -> assert false
    | td' ->
      let (_,ty') = type_def td' in
      if reloc_dummy ty' <> reloc_dummy ty then
        fail "name %S cannot refer to different types %s and %s" name (show_type_def td) (show_type_def td')

  (* atdgen does topological sorting itself, but still lets try to keep natural order *)
  let replace t name ty =
    match List.partition (function ty' -> type_def_name ty' = name) !(t.types) with
    | [Atd.Ast.Type (loc,(n,tp,annot),_)], rest -> t.types := (Type (loc,(n,tp,annot),ty)) :: rest
    | _ -> assert false

  let ref_ t ?name ty =
    let ty = reloc_dummy ty in
    match ty with
    | Atd.Ast.Record _ ->
      begin match List.find (fun (Atd.Ast.Type (_,_,v)) -> ty = reloc_dummy v) !(t.types) with
      | Atd.Ast.Type (_,(name,[],_),_) -> tname name
      | _ -> assert false (* parametric type cannot match *)
      | exception _ ->
        let name = fresh_name t ?name ty in
        new_ t (typ name ty);
        tname name
      end
    | _ -> ty

  (* remove nested records, producing more types as needed *)
  let unrecord t name ty =
    let rec map name ty =
      match ty with
      | Atd.Ast.Record (loc, fields, annot) -> ref_ t ?name (map_record (loc, fields, annot))
      | Sum (_loc, _variants, _annot) -> fail "variants are not supported"
      | Tuple (loc, cells, annot) -> Tuple (loc, List.map (fun (loc,ty,annot) -> loc, map None ty, annot) cells, annot)
      | List (loc, ty, annot) ->
        let name =
          match name with
          | Some name when String.ends_with name "s" && String.length name > 1 -> Some (String.slice ~last:(-1) name)
          | Some name -> Some (name ^ "_elem")
          | None -> None
        in
        List (loc, map name ty, annot)
      | Option (loc, ty, annot) -> Option (loc, map name ty, annot)
      | Nullable (loc, ty, annot) -> Nullable (loc, map name ty, annot)
      | Shared (loc, ty, annot) -> Shared (loc, map name ty, annot)
      | Wrap (loc, ty, annot) -> Wrap (loc, map name ty, annot)
      | Name (loc, (loc', n, tl), annot) ->
        let name = match n, tl with "buckets", [_] -> name | _ -> None in
        Name (loc, (loc', n, List.map (map name) tl), annot)
      | Tvar _ -> ty
    and map_record (loc,fields,annot) =
      let fields = fields |> List.map begin function
      | `Inherit _ -> fail "inherit not supported"
      | `Field (loc,(name,_,_ as f),ty) -> `Field (loc, f, map (Some name) ty)
      end
      in
      Record (loc,fields,annot)
    in
    match ty with
    | Atd.Ast.Record (loc, fields, annot) -> map_record (loc, fields, annot) (* record type at top-level, no need to unnest *)
    | _ -> map (Some name) ty

  let get t =
    !(t.types) |> List.rev |> List.iter (function td -> let (name,ty) = type_def td in replace t name (unrecord t name ty));
    List.rev !(t.types)

end

let make_abstract ((_loc,annot),init) types =
  types |> List.map begin fun t ->
    match List.find (fun i -> type_def_name i = type_def_name t) init with (* match by name, because initial types are not renamed *)
    | exception Not_found -> t
    | _ -> Atd.Ast.Type (loc, (type_def_name t,[],annot),tname "abstract")
  end

let safe_record map fields =
  fields |> List.map begin fun (name,t) ->
    let kind = match t with Maybe _ -> Atd.Ast.Optional | List _ -> With_default | _ -> Required in
    let (a,name) = safe_ident name in (* TODO check unique *)
    field ~a ~kind name (map t)
  end |> record

let basic_json = typ "basic_json" ~a:["ocaml",["module","Json";"t","t"]] (tname "abstract")

let add_shape t name shape =
  Types.add t @@ ptyp "doc_count" ["key"] (record [field "key" (tvar "key"); field "doc_count" (tname "int")]);
  Types.add t @@ ptyp "buckets" ["a"] (record [field "buckets" (list (tvar "a"))]);
  Types.add t @@ typ "int_as_float" (wrap (tname "float") ["t","int"; "wrap","int_of_float"; "unwrap","float_of_int"]);
  Types.add t @@ ptyp "value_agg'" ["a"] (record [field "value" (tvar "a")]);
  Types.add t @@ ptyp "value_agg" ["a"]
    (wrap (pname "value_agg'" [tvar "a"]) [ "t", "'a"; "wrap", "fun { value; } -> value"; "unwrap", "fun value -> { value; }"]);
(*
  Types.add t @@ ptyp "value_as_string_agg'" ["a"] (record [field "value" (tvar "a")]);
  Types.add t @@ ptyp "value_as_string_agg" ["a"]
    (wrap (pname "value_as_string_agg'" [tvar "a"]) [ "t", "'a"; "wrap", "fun { value_as_string = v; } -> v"; "unwrap", "fun v -> { value_as_string = v; }"]);
*)
  Types.add t @@ basic_json;
  let rec map shape =
    match shape with
    | Simple t -> of_simple_type t
    | Maybe t -> nullable @@ map t
    | Ref (ref,t) -> wrap_ref ref (of_simple_type t)
    | List t -> list (map t)
    | List_or_single (Ref (ref,_)) -> wrap_ref (ES_name.append ref "list_or_single") (tname "basic_json");
    | List_or_single t -> fail "cannot handle mixed_multi of %s" (show_result_type t)
    | Object t -> list ~a:["json",["repr","object"]] (tuple [map (Simple String); map t])
    | Dict ["key",k; "doc_count", Simple Int] -> pname "doc_count" [map k]
    | Dict ["buckets", List t] -> pname "buckets" [map t]
    | Dict ["value", Dict ["override int as float hack", Simple Int]] -> pname "value_agg" [tname "int_as_float"]
    | Dict ["value", t] -> pname "value_agg" [map t]
(*     | Dict ["value_as_string", t] -> pname "value_as_string_agg" [map t] *)
    | Dict fields -> safe_record map fields
  in
  Types.add t @@ typ name (map shape)

let add_vars t (l:input_vars) =
  Types.add t basic_json;
  let rec map_field (req,t) : (_ * Atd.Ast.type_expr) =
    let t =
      match t with
      | `Group l -> map l
      | `Simple t ->
        match t with
        | None -> tname "basic_json"
        | Some t -> of_var_type t
    in
    match req with
    | `Required -> Atd.Ast.Required, t
    | `Optional -> Optional, nullable t
  and map l =
    match l with
    | [] -> tname "unit"
    | _ -> l |> List.map begin fun (name,value) ->
      let (a,name) = safe_ident name in (* TODO check unique *)
      let (kind,t) = map_field value in
      field ~a ~kind name t
     end |> record
  in
  Types.add t @@ typ "input" (map l)

let make_module ~init f : Atd.Ast.full_module =
  let t = Types.empty () in
  List.iter (Types.add t) (snd init);
  f t;
  (loc,[]), (make_abstract init (Types.get t))

let of_shape ~init name shape = make_module ~init (fun t -> add_shape t name shape)
let of_vars ~init (l:input_vars) = make_module ~init (fun t -> add_vars t l)
let make ~init l name shape = make_module ~init (fun t -> add_vars t l; add_shape t name shape)

let parse_file filename =
  let open Atd in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  Parser.full_module Lexer.token lexbuf
