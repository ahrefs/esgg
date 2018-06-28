(** Generating atd *)

open Common

val of_vars : input_vars -> Atd_ast.full_module

val of_shape : string -> result_type -> Atd_ast.full_module
