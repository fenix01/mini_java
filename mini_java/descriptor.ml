open Ast
open Mips

let str_type type_ =
	match type_ with
	| Tnull -> "null"
	| Tint -> "int"
	| Tboolean -> "boolean"
	| Tclass cname -> cname
	| Tvoid -> "void"

(* GENERATION DES DESCRIPTEURS DE CLASSES *)
let default_desc =
	label "_desc$Object" @@ dword [0] @@
	label "_desc$String" @@  address ["_desc$Object"] @@ address ["_method$String$equals$Object"]
	
let class_desc class_name = "_desc$"^class_name

let method_desc class_name method_name method_type params =
	let prolog = method_type ^ class_name ^ "$" ^ method_name in
			let epilog = List.fold_left (
						fun desc param ->
								let type_, _ = param in
								desc ^ "$" ^ str_type type_
					) "" params
			in prolog ^ epilog

let rec generate_descriptor class_name defns =
	match defns with
	| [] -> nop
	| el :: r ->
			let defn_descriptor =
				match el with
				| Dconstr (f, params, _ ) -> address [ method_desc class_name f.node "_cotr$" params ]
				| Dmeth (ret, f, params, _) -> address [ method_desc class_name f.node "_method$" params ]
				| _ -> nop
			in defn_descriptor @@ generate_descriptor class_name r

let generate_descriptors class_list =
	let rec descriptors clist =
		match clist with
		| [] -> nop
		| (this, parent, defns) :: r -> label (class_desc this.node) @@ 
																		address [class_desc parent.node] @@
																		generate_descriptor this.node defns @@ descriptors r
	in comment "liste des descripteurs de classes" @@ descriptors class_list
	
(*########################################################*)