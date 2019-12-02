(** Inferring types of input and output *)

open Common

val derive : mapping -> Tjson.t -> Query.t * input_vars * (string -> string) * (string * string * string * Tjson.t option)

val output : mapping -> Tjson.t -> result_type

val print_reflect : string -> Yojson.Basic.t -> unit
