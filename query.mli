(** Parsing search query *)

open Common

type query_t
type var_list
type query = { json : Tjson.t; query : query_t }
type t =
| Search of { q : query; extra : constraint_t list; source : source_filter option; }
| Mget of var_list
| Get of (Tjson.var * source_filter option)

module Variable : sig

type t = Property of multi * ES_name.t * simple_type | Any | Type of simple_type | List of simple_type

end

val extract_query : Tjson.t -> query

val infer' : constraint_t list -> query -> constraint_t list
val infer : query -> constraint_t list

val extract : Tjson.t -> t

val resolve_constraints : mapping -> constraint_t list -> (string, Variable.t) ExtLib.Hashtbl.t

val resolve_mget_types : var_list -> (string, Variable.t) ExtLib.Hashtbl.t
val resolve_get_types : Tjson.var -> (string, Variable.t) ExtLib.Hashtbl.t
