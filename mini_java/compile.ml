open Ast
open Mips

let rec compile_expr exp =
	match i with
  | Econst (c) -> assert false
	| _ -> assert false

let rec compile_instr instr =
	match instr with
	| Idecl (t, x, eopt) ->
	| _ -> assert false 
	
let prog (class_decl,main_name,main_body) =
	let body_code = compile_instr main_body in
	{
		text =
			label "main"
		@@ comment "c'est le main"
		@@ body_code
		@@ li v0 10
		@@ syscall
	}