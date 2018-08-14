(** Generating atd *)

open Atd
open Common

val of_vars : init:Ast.module_body -> input_vars -> Ast.full_module

val of_shape : init:Ast.module_body -> string -> result_type -> Ast.full_module

val parse_file : string -> Ast.module_body
