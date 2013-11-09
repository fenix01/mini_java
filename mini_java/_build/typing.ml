open Ast
open Type_class

exception Instruction_error of string * position
exception Expression_error of string

let error s p = raise (Instruction_error (s, p))

module Env = Map.Make(String)
let env = Env.add "System" (Tclass "System") Env.empty

let is_class env =
	Env.mem "this" env

let rec subtype t1 t2 fclass_table =
	match t1, t2 with
	| Tvoid, Tvoid -> true
	| Tboolean, Tboolean -> true
	| Tint, Tint -> true
	| Tnull, Tnull -> true
	| Tnull , Tclass _ -> true
	| Tclass c1, Tclass c2 ->
			let c1_ = get_class c1 fclass_table in
			if c1 = c2 || is_parent c2 c1_ fclass_table then true else false
	| _, _ -> false

let compatible t1 t2 fclass_table = subtype t1 t2 fclass_table || subtype t2 t1 fclass_table
let compatible_strict t1 t2 fclass_table = subtype t1 t2 fclass_table

(* vérifie que sign1 est sous type de sign2 t1 = méthode étudiée, t2 =     *)
(* méthode de classe                                                       *)
let compare_signature sign1 sign2 fclass_table =
	try
		List.fold_left2 (
				fun comp t1 t2 ->
						if compatible_strict t1 t2 fclass_table then
							comp && true
						else comp && false
			) true sign1 sign2
	with Invalid_argument _ -> raise (Expression_error ("aucune méthode de classe n'existe pour la signature correspondante."))

(* select_field permet de connaître le type d'un attribut il suffit de     *)
(* fournir le nom de la classe et le nom de l'attribut                     *)
let select_field class_name attribute_name class_table =
	let class_ = get_class class_name class_table in
	try
		List.assoc attribute_name class_.attributes
	with
	| Not_found -> raise (Expression_error ("l'identificateur " ^ attribute_name ^ " n'existe pas"))

(* select_cotrs permet de connaître le type d'une méthode. Prend le nom de *)
(* la classe et le nom de la méthode                                       *)
let select_cotrs class_name signature class_table =
	let class_info = get_class class_name class_table in
	if List.length class_info.cotrs > 0 then
		let check = List.fold_left (
					fun found cotr_ ->
							let params_, _ = cotr_ in
							let cotr_sign = get_lparams_signature params_ in
							(compare_signature signature cotr_sign class_table, Tclass class_name)
				) (false, Tnull) class_info.cotrs in
		if fst check then snd check else
			raise (Expression_error ("aucun constructeur pour la classe "^
						class_name ^ " n'existe pour la signature correspondante."))
	else
		raise (Expression_error ("le constructeur n'existe pas dans la classe " ^ class_name))

(* select_method permet de connaître le type d'une méthode. Prend le nom   *)
(* de la classe et le nom de la méthode                                    *)
let select_method class_name method_name signature class_table =
	let class_info = get_class class_name class_table in
	let methods_ = List.find_all (
				fun method_ -> let name_, _ = method_ in
						if name_ = method_name then true else false
			) class_info.methods in
	if List.length methods_ > 0 then
		let check = List.fold_left (
					fun found method_ ->
							let name_, (type_, params_, class_name, _) = method_ in
							let meth_sign = get_lparams_signature params_ in
							(compare_signature signature meth_sign class_table, type_)
				) (false, Tnull) methods_ in
		if fst check then snd check else
			raise (Expression_error ("aucune méthode de classe n'existe pour la signature correspondante."))
	else
		raise (Expression_error ("la méthode " ^ method_name ^ " n'existe pas dans la classe " ^ class_name))

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
					if compatible_strict type_exp type_var fclass_table then
						type_var
					else
						raise (Expression_error ("types incompatibles lors de l'assignation."))
				with Expression_error msg -> raise (Expression_error msg)
			)
	
	| Ecall (l, elist) -> type_lvalue2 env l elist fclass_table
	| Enew (id, elist) -> 
		if not (class_exists id.node fclass_table) then
								raise (Expression_error "le type de cette classe n'existe pas")
							else
								let class_info = get_class id.node fclass_table in
								let signature = create_signature elist env fclass_table in
								select_cotrs class_info.name signature fclass_table
	| Eunop(op, exp) ->
			(try
				let type_ = type_expr env exp fclass_table in
				match op with
				| Unot -> if compatible type_ Tboolean fclass_table then
							Tboolean
						else
							raise (Expression_error "l'expression doit être de type boolean.")
				| _ ->
						if compatible type_ Tint fclass_table then
							Tint
						else
							raise (Expression_error "les expressions doivent être de types int.")
			with Expression_error msg -> raise (Expression_error msg))
	| Ebinop (e1, op, e2) ->
			let t1 = type_expr env e1 fclass_table in
			let t2 = type_expr env e2 fclass_table in
			(
				match op with
				| Beq | Bneq -> if compatible t1 t2 fclass_table then
							Tboolean
						else
							raise (Expression_error "les types comparés sont incompatibles")
				| Band | Bor ->
						if compatible t1 Tboolean fclass_table && compatible t2 Tboolean fclass_table then
							Tboolean
						else
							raise (Expression_error "les expressions doivent être de types boolean.")
				| Blt | Blte | Bgt | Bgte ->
						if compatible t1 Tint fclass_table && compatible t2 Tint fclass_table then
							Tboolean
						else
							raise (Expression_error "les expressions doivent être de types int.")
				| Bsub | Bmul | Bdiv | Bmod ->
						if compatible t1 Tint fclass_table && compatible t2 Tint fclass_table then
							Tint
						else
							raise (Expression_error "les expressions doivent être de types int.")
				| Badd ->
						match t1, t2 with
						| Tclass "String", ((Tclass "String") | Tint) -> Tclass "String"
						| Tint, Tclass "String" -> Tclass "String"
						| Tint , Tint -> Tint
						| _ , _ -> raise (Expression_error "addition impossible entre ces deux expressions.")
				
			)
	| Einstanceof (e, t) ->
			let type_e = type_expr env e fclass_table in
			(match type_e with
				| (Tclass _) | Tnull ->
						if not (check_classvar type_e fclass_table) then
							raise (Expression_error "le type de cette classe n'existe pas")
						else
						if compatible type_e t fclass_table then
							Tboolean
						else raise (Expression_error "les types comparés sont incompatibles")
				| _ -> raise (Expression_error "instanceof ne s'applique qu'avec des types classes, ou type null") )
	| Ecast (t, e) ->
			let type_e = type_expr env e fclass_table in
			if not (check_classvar t fclass_table) then
				raise (Expression_error "le type de cette classe n'existe pas")
			else
				if compatible type_e t fclass_table then
					t
				else
					raise (Expression_error "type incompatible lors de la tentative de cast")

and create_signature params env fclass_table =
	List.fold_left ( fun x exp ->
					try
						let type_ = type_expr env exp fclass_table in
						type_:: x
					with Expression_error msg -> raise (Expression_error msg)
		) [] params

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
							else if x.node = "this" then
								raise (Expression_error "this n'existe pas dans le contexte actuel.")
							else raise (Expression_error ("l'identificateur " ^ x.node ^ " n'existe pas."))
						with Not_found ->	raise (Expression_error ("l'identificateur " ^ x.node ^ " n'existe pas."))
			)
	| Laccess (e, x) ->
			(
				try
					let type_e = type_expr env e fclass_table in
					match type_e with
					| Tclass class_name ->
							if not (check_classvar type_e fclass_table) then
								raise (Expression_error "le type de cette classe n'existe pas")
							else
								let class_info = get_class class_name fclass_table in
								let field_ = select_field class_info.name x.node fclass_table in fst field_
					| _ -> raise (Expression_error ("l'accès à un identifiant nécessite un type classe"))
				with
					Not_found ->	raise (Expression_error ("l'expression d'accès est incorrect"))
			)
and type_lvalue2 env l params fclass_table =
	match l with
	| Lident x -> (
				try
					Env.find x.node env
				with
					Not_found ->
						if is_class env then
							let class_info = this_to_classinfo env fclass_table in
							let method_name = x.node in
							let signature = create_signature params env fclass_table in
							select_method class_info.name method_name signature fclass_table
						else
							raise (Expression_error "cette méthode n'existe pas.")
			)
	| Laccess (e, x) ->
			(
				try
					let type_e = type_expr env e fclass_table in
					match type_e with
					| Tclass class_name ->
							if not (check_classvar type_e fclass_table) then
								raise (Expression_error "le type de cette classe n'existe pas")
							else
								let class_info = get_class class_name fclass_table in
								let method_name = x.node in
								let signature = create_signature params env fclass_table in
								select_method class_info.name method_name signature fclass_table
					| _ -> raise (Expression_error ("l'accès à un identifiant nécessite un type classe"))
				with
					Not_found ->	raise (Expression_error ("l'expression d'accès est incorrect"))
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
				error ("l'identificateur " ^ id.node ^ " est déjà défini.") id.info
			else
				Env.add id.node t env
	| Idecl(t, id, Some e) ->
			if not (check_classvar t fclass_table) then
				error ("le type de cette classe n'existe pas") id.info
			else
			if Env.mem id.node env then
				error ("l'identificateur " ^ id.node ^ " est déjà défini.") id.info
			else
				(
					try
						let type_ = type_expr env e fclass_table in
						let check = compatible type_ t fclass_table in
						if not check then
							error ("la variable " ^ id.node ^ " est mal typée.") id.info
						else Env.add id.node t env
					with Expression_error msg -> error msg id.info
				)
	| Iif (e1, in1, in2) ->
			(try
				let t1 = type_expr env e1 fclass_table in
				if t1 <> Tboolean
				then error ("l'expression n'est pas booléenne.") e1.info
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
									if compatible t1 Tint fclass_table && compatible t2 Tboolean fclass_table && compatible t3 Tint fclass_table
									then
										env
									else
										error ("la boucle for est mal typée.") ex1.info
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

(* créé l'environnement pour l'identificateur this *)
let create_class_env env class_info =
	Env.add "this" (Tclass class_info.name) env

(* créé l'environnement pour les paramètres *)
let create_params_env env params =
	List.fold_left (
			fun new_env (type_, name_) ->
					Env.add name_ type_ new_env
		) env params

(* permet le typage de toutes les méthodes d'une classe *)
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

(* effectue le typage sur toutes les classes excepté la classe main *)
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