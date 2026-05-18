(** Generating atd *)

open Atd
open Common

val of_vars : inits:Ast.full_module list -> input_vars -> Ast.full_module

val of_shape : inits:Ast.full_module list -> string -> result_type -> Ast.full_module

val make : inits:Ast.full_module list -> input_vars -> string -> result_type -> Ast.full_module

val parse_file : string -> Ast.full_module

val parse_string : string -> Ast.full_module
