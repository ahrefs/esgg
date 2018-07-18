(** Inferring types of input and output *)

open Common

val derive : mapping -> Tjson.t -> input_vars * (string -> string) * Tjson.t

val output : mapping -> Tjson.t -> Atd.Ast.full_module

val print_reflect : string -> Yojson.Basic.json -> unit
