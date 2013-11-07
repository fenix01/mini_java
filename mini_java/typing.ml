open Ast
open Type_class

exception Instruction_error of string * position
exception Expression_error of string

let error s p = raise (Instruction_error (s, p))

module Env = Map.Make(String)
let env = Env.empty

(* select_field permet de conna�tre le type d'un attribut il suffit de     *)
(* fournir le nom de la classe et le nom de l'attribut                     *)
let select_field class_name attribute_name class_table =
	let class_ = get_class class_name class_table in
	try
		List.assoc attribute_name class_.attributes
	with
	| Not_found -> raise (Expression_error ("l'identificateur " ^ attribute_name ^ " n'existe pas"))

let this_to_classinfo env class_table =
	try
		let type_ = Env.find "this" env in
		let class_name = match type_ with
			| Tclass a -> a
			| _ -> ""
		in Hashtbl.find class_table class_name
	with Not_found -> raise (Expression_error "this n'existe pas dans le contexte actuel.")

let check_classvar type_ class_table =
	match type_ with
	| Tclass class_name -> if Hashtbl.mem class_table class_name then true else false
	| _ -> true

let is_class env =
	Env.mem "this" env

let rec subtype t1 t2 =
	match t1, t2 with
	| Tvoid, Tvoid -> true
	| Tboolean, Tboolean -> true
	| Tint, Tint -> true
	| Tnull, Tnull -> true
	| (Tclass "Object" | Tclass "String"), (Tclass "Object" | Tclass "String") -> true
	| Tnull , Tclass _ -> true
	| Tclass c1, Tclass c2 -> if c1 = c2 then true else false
	| _, _ -> false

let compatible t1 t2 = subtype t1 t2 || subtype t2 t1

let is_string t =
	match t with
	| Tclass "String" -> true
	| _ -> false

(* let findClass env c = Env.mem c env let isClass env c = try findClass   *)
(* env c with | Not_found-> false                                          *)

(* typage d'une expression pour chaque expression, on commence par typer   *)
(* les sous-expression pour typer l'expression de resultat                 *)
let rec type_expr env e fclass_table =
	match e.node with
	| Econst c -> (
				match c with
				| Cint c -> Tint
				| Cstring s -> Tclass "String"
				| Cbool b -> Tboolean
				| Cnull -> Tnull)
	
	| Elval e -> type_lvalue env e fclass_table
	| Eassign (l, exp) ->
			(
				try
					let type_var = type_lvalue env l fclass_table in
					let type_exp = type_expr env exp fclass_table in
					if compatible type_var type_exp then
						type_exp
					else
						raise (Expression_error ("types incompatibles lors de l'assignation."))
				with Expression_error msg -> raise (Expression_error msg)
			)
	
	| Ecall (l, elist) ->
	(* let ret_typ param_list = try Env.find l.node fun_env with Not_found   *)
	(* -> failwith "type call inconnue" in begin try let args0 = List.map2   *)
	(* (fun e0 (tx,_) -> let et0 = type_expr env e0 in if compatible         *)
	(* et0.node tx then et0 else failwith "type Ecall error" )args           *)
	(* param_list in add_node ret_typ (Ecall (l,args0)) with                 *)
	(* Invalide_argument _ -> failwith "function %s expects %i arguments but *)
	(* was called with %i" end                                               *)
			failwith "todo"
	| Enew (id, elist) -> failwith "todo"
	| Eunop(op, exp) ->
			(try
				let type_ = type_expr env exp fclass_table in
				match op with
				| Unot -> if compatible type_ Tboolean then
							Tboolean
						else
							raise (Expression_error "l'expression doit �tre de type boolean.")
				| _ ->
						if compatible type_ Tint then
							Tint
						else
							raise (Expression_error "les expressions doivent �tre de types int.")
			with Expression_error msg -> raise (Expression_error msg))
	| Ebinop (e1, op, e2) ->
			let t1 = type_expr env e1 fclass_table in
			let t2 = type_expr env e2 fclass_table in
			(
				match op with
				| Beq | Bneq -> if compatible t1 t2 then
							Tboolean
						else
							raise (Expression_error "les types compar�s sont incompatibles")
				| Band | Bor ->
						if compatible t1 Tboolean && compatible t2 Tboolean then
							Tboolean
						else
							raise (Expression_error "les expressions doivent �tre de types boolean.")
				| Blt | Blte | Bgt | Bgte ->
						if compatible t1 Tint && compatible t2 Tint then
							Tboolean
						else
							raise (Expression_error "les expressions doivent �tre de types int.")
				| Bsub | Bmul | Bdiv | Bmod ->
						if compatible t1 Tint && compatible t2 Tint then
							Tint
						else
							raise (Expression_error "les expressions doivent �tre de types int.")
				| Badd -> 
					match t1,t2 with
					| Tclass "String",((Tclass "String") | Tint) -> Tclass "String"
					| Tint,Tclass "String" -> Tclass "String"
					| Tint , Tint -> Tint
					| _ , _ -> raise (Expression_error "addition impossible entre ces deux expressions.")	
								
			)
	| Einstanceof (e, t) -> failwith "todo"
	(* let te = type_expr env e in if (te.info = Tnull || isClass te) then   *)
	(* if compatible te.info t then add_node Tboolean (Einstanceof (te,t))   *)
	(* else failwith "type error einstanceof" else failwith " te should be a *)
	(* class or null"                                                        *)
	| Ecast (t, e) -> failwith "todo"
(* if (not(Env.mem (stringOf t) env)) then failwith "t should be a type    *)
(* Tclass --Ecast" else let te = type_expr env e in if ((subtype t         *)
(* te.info)|| (subtype te.info t)) then add_node te.info (Ecast(t,te))     *)
(* else failwith "typing error Ecast "                                     *)

and type_lvalue env l fclass_table =
	match l with
	| Lident x -> (
				try
					Env.find x.node env
				with
					Not_found ->
						try
							if is_class env then
								let class_info = this_to_classinfo env fclass_table in
								let field_ = select_field class_info.name x.node fclass_table in fst field_
							else
								raise (Expression_error ("l'identificateur " ^ x.node ^ " n'existe pas."))
						with Not_found ->	raise (Expression_error ("l'identificateur " ^ x.node ^ " n'existe pas."))
				)
	| Laccess (e, x) -> (
				try
					let type_e = type_expr env e fclass_table in
					match type_e with
					| Tclass class_name -> 
						if not (check_classvar type_e fclass_table) then
							raise (Expression_error "le type de cette classe n'existe pas")
						else
							let class_info = get_class class_name fclass_table in
							let field_ = select_field class_info.name x.node fclass_table in fst field_
					| _ -> raise (Expression_error ("l'acc�s � un identifiant n�cessite un type classe"))
				with
					Not_found ->	raise (Expression_error ("l'expression d'acc�s est incorrect"))
			)

let rec type_instr env i fclass_table =   (* : type_instr Env.empty main_body *)
	match i.node with
	| Iexpr e ->
			(try
				let _ = type_expr env e fclass_table in
				env
			with Expression_error msg -> error msg i.info)
	| Idecl(t, id, None) ->
			if not (check_classvar t fclass_table) then
				error ("le type de cette classe n'existe pas") id.info
			else
			if Env.mem id.node env then
				error ("l'identificateur " ^ id.node ^ " est d�j� d�fini.") id.info
			else
				Env.add id.node t env
	| Idecl(t, id, Some e) ->
			if not (check_classvar t fclass_table) then
				error ("le type de cette classe n'existe pas") id.info
			else
			if Env.mem id.node env then
				error ("l'identificateur " ^ id.node ^ " est d�j� d�fini.") id.info
			else
				(
					try
						let type_ = type_expr env e fclass_table in
						let check = compatible type_ t in
						if not check then
							error ("la variable " ^ id.node ^ " est mal typ�e.") id.info
						else Env.add id.node t env
					with Expression_error msg -> error msg id.info
				)
	| Iif (e1, in1, in2) ->
			(try
				let t1 = type_expr env e1 fclass_table in
				if t1 <> Tboolean
				then error ("l'expression n'est pas bool�enne.") e1.info
				else
					(
						let _ = type_instr env in1
						and _ = type_instr env in2
						in
						env
					)
			with Expression_error msg -> error msg e1.info)
	| Ifor (e1, e2, e3, inst) ->
			(match e1, e2, e3 with
				| Some ex1, Some ex2, Some ex3 ->
						(try
							let t1 = type_expr env ex1 fclass_table in
							(try
								let t2 = type_expr env ex2 fclass_table in
								(try
									let t3 = type_expr env ex3 fclass_table in
									let _ = type_instr env inst fclass_table in
									if compatible t1 Tint && compatible t2 Tboolean && compatible t3 Tint
									then
										env
									else
										error ("la boucle for est mal typ�e.") ex1.info
								with Expression_error msg -> error msg ex3.info
								)
							with Expression_error msg -> error msg ex2.info
							)
						with Expression_error msg -> error msg ex1.info
						)
				| _ , _ , _ -> env
			)
	
	| Iblock(in_list) ->
			let rec aux env list =
				match list with
				| [] -> env
				| a :: r -> let new_env = type_instr env a fclass_table in aux new_env r
			in aux env in_list
	| Ireturn _ -> failwith "todo2"

(* cr�� l'environnement pour l'identificateur this *)
let create_class_env env class_info =
	Env.add "this" (Tclass class_info.name) env

(* cr�� l'environnement pour les param�tres *)
let create_params_env env params =
	List.fold_left (
			fun new_env (type_, name_) ->
					Env.add name_ type_ new_env
		) env params

(* permet le typage de toutes les m�thodes d'une classe *)
let type_methods classes_ =
	let class_info, fclass_table = classes_ in
	List.iter (
			fun method_ ->
					let class_env = create_class_env env class_info in
					match method_ with
					| _, ( _, params_, _, Some instr) ->
							let params_env = create_params_env class_env params_ in
							let _ = type_instr params_env instr fclass_table in ()
					| _, ( _, _, _, None) -> ()
		) class_info.methods

(* permet le typage de tous les constructeurs d'une classe *)
let type_cotrs classes_ =
	let class_info, fclass_table = classes_ in
	List.iter (
			fun cotr_ ->
					let class_env = create_class_env env class_info in
					match cotr_ with
					| params_, Some instr ->
							let params_env = create_params_env class_env params_ in
							let _ = type_instr params_env instr fclass_table in ()
					| _, None -> ()
		) class_info.cotrs

(* effectue le typage sur toutes les classes except� la classe main *)
let type_classes fclass_table =
	Hashtbl.iter (
			fun class_name class_info ->
					type_methods (class_info, fclass_table);
					type_cotrs (class_info, fclass_table)
		) fclass_table

(* effectue le typage sur l'ensemble du programme *)
let type_prog prog =
	let table = init_table prog in
	let _, _ , main_body = prog in
	let _ = type_instr env main_body table in
	type_classes table