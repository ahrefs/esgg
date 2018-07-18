(** Generating atd *)

open Common

val of_vars : input_vars -> Atd.Ast.full_module

val of_shape : string -> result_type -> Atd.Ast.full_module
