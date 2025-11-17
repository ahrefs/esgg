(** Parsing search query *)

open Common

type query_t
type var_list
type query = { json : Tjson.t; query : query_t; cstrs : constraint_t list; }
type t =
| Search of { q : query; extra : constraint_t list; source : source_filter or_var option; fields : string list option; highlight : string list option; json: Tjson.t }
| Mget of { ids: var_list; json: Tjson.t; conf: Tjson.t }
| Get of { id : Tjson.var; return : [ `Source of source_filter | `Fields of string list | `Nothing ]; json: Tjson.t }

module Variable : sig

type t = Property of cardinality * ES_name.t * simple_type | Any | Type of simple_type | List of simple_type

end

val extract_query : Tjson.t -> query
val extract_source : Tjson.t -> source_filter or_var option
val extract_source_static : Tjson.t -> source_filter option
val extract_highlight : Tjson.t -> string list option
val extract_inner_hits : Tjson.t -> inner_hits_spec option
val extract_inner_hits_from_query : query -> (string * inner_hits_spec) list
val extract_inner_hits_from_conf : Tjson.t -> (string * inner_hits_spec) list
val has_matched_queries : Tjson.t -> bool

val infer' : constraint_t list -> query -> constraint_t list
val infer : query -> constraint_t list

val extract : Tjson.t -> t

val resolve_constraints : mapping -> constraint_t list -> (string, Variable.t) ExtLib.Hashtbl.t

val resolve_mget_types : var_list -> (string, Variable.t) ExtLib.Hashtbl.t
val resolve_get_types : Tjson.var -> (string, Variable.t) ExtLib.Hashtbl.t
