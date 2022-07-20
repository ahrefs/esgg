open Common

val analyze : mapping -> Tjson.t -> Tjson.t * (constraint_t list * (string * result_type)) list

val derive_highlight : mapping -> string list -> result_type
val derive_fields : mapping -> string list -> (string * result_type) list
