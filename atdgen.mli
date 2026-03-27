(** Generating atd *)

open Common

type atd_module = Atd.Ast.module_head * Atd.Ast.module_body

val of_vars : init:atd_module -> input_vars -> atd_module

val of_shape : init:atd_module -> string -> result_type -> atd_module

val make : init:atd_module -> input_vars -> string -> result_type -> atd_module

val parse_file : string -> atd_module
