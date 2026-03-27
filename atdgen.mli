(** Generating atd *)

open Common

val of_vars : init:Atd.Ast.full_module -> input_vars -> Atd.Ast.full_module

val of_shape : init:Atd.Ast.full_module -> string -> result_type -> Atd.Ast.full_module

val make : init:Atd.Ast.full_module -> input_vars -> string -> result_type -> Atd.Ast.full_module

val parse_file : string -> Atd.Ast.full_module
