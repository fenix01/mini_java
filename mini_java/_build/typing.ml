open Ast
open Type_class

exception Instruction_error of string * position
exception Expression_error of string

let error s p = raise (Instruction_error (s, p))

module Env = Map.Make(String)
let env = Env.empty

let rec subtype t1 t2 =
	match t1, t2 with
	| Tvoid, Tvoid -> true
	| Tboolean, Tboolean -> true
	| Tint, Tint -> true
	| Tnull, Tnull -> true
	| (Tclass "Object" | Tclass "String"), (Tclass "Object" | Tclass "String") -> true
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
let rec type_expr env e =
	match e.node with
	| Econst c -> (
				match c with
				| Cint c -> Tint
				| Cstring s -> Tclass "String"
				| Cbool b -> Tboolean
				| Cnull -> Tnull)
	
	| Elval e -> type_lvalue env e
	| Eassign (l, exp) ->
			(
				try
					let type_var = type_lvalue env l in
					let type_exp = type_expr env exp in
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
			let type_ = type_expr env exp in
			(* match inutile ici car les opérateurs unaires ne prennent que des ints *)
			if compatible type_ Tint then
				Tint
			else
				raise (Expression_error "les expressions doivent être de types int.")
		with Expression_error msg -> raise (Expression_error msg))
	| Ebinop (e1, op, e2) ->
			let t1 = type_expr env e1 in
			let t2 = type_expr env e2 in
			(
				print t1;
				print t2;
				match op with
				| Band | Bor | Bneq ->
					if compatible t1 Tboolean && compatible t2 Tboolean then
						Tboolean
					else
						raise (Expression_error "les expressions doivent être de types boolean.")
				| Beq  | Blt | Blte | Bgt | Bgte ->
						(if compatible t1 t2 then
								Tboolean
							else
								raise (Expression_error "les types comparés sont incompatibles"))
				| Badd | Bsub | Bmul | Bdiv | Bmod ->
						if compatible t1 Tint && compatible t2 Tint then
							Tint
						else
							raise (Expression_error "les expressions doivent être de types int.")
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

and type_lvalue env l =
	match l with
	| Lident x -> (try Env.find x.node env
			with Not_found -> raise (Expression_error ("l'identificateur " ^ x.node ^ " n'existe pas.") ))
	| Laccess (e, x) ->
			try
				let t = type_expr env e in
				match t with
				| _ -> failwith "..."
				| _ -> failwith "..."
			with Expression_error msg -> raise (Expression_error msg)

let rec type_instr env i =   (* : type_instr Env.empty main_body *)
	match i.node with
	| Iexpr e ->
			(try
				let _ = type_expr env e in
				env
			with Expression_error msg -> error msg i.info)
	| Idecl(t, id, None) -> if Env.mem id.node env then
				error ("la variable " ^ id.node ^ " est déjà défini") id.info
			else
				Env.add id.node t env
	| Idecl(t, id, Some e) -> if Env.mem id.node env then
				error ("la variable " ^ id.node ^ " est déjà défini") id.info
			else
				(
					try
						let type_ = type_expr env e in
						let check = compatible type_ t in
						if not check then
							error ("la variable " ^ id.node ^ " est mal typée") id.info
						else Env.add id.node t env
					with Expression_error msg -> error msg id.info
				)
	| Iif (e1, in1, in2) ->
			(try
				let t1 = type_expr env e1 in
				if t1 <> Tboolean
				then error ("l'expression n'est pas booléenne") e1.info
				else
					(
						let _ = type_instr env in1
						and _ = type_instr env in2
						in
						env
					)
			with Expression_error msg -> error msg e1.info)
	| Ifor (Some e1, Some e2, Some e3, inst) ->
			(try
				let t1 = type_expr env e1 in
				(try
					let t2 = type_expr env e2 in
					(try
						let t3 = type_expr env e3 in
						let _ = type_instr env inst in
						if compatible t1 Tint && compatible t2 Tboolean && compatible t3 Tint
						then
							env
						else 
							error ("la boucle for est mal typée.") e1.info
						with Expression_error msg -> error msg e3.info
						)
				with Expression_error msg -> error msg e2.info
				)
			with Expression_error msg -> error msg e1.info
			)
	| Iblock(in_list) ->
			let rec aux env list =
				match list with
				| [] -> env
				| a :: r -> let new_env = type_instr env a in aux new_env r
			in aux env in_list
	| Ireturn _ -> failwith "todo2"
	| _ -> error "instruction non gérée." i.info

(* and type_block env il sl = (* let il0, env0 = type_decl env il in *)    *)
(* let sl0 = List.map(type_instr env) sl in il0, sl0                       *)

let type_prog prog =
	init_table prog;
	let clist, name , main_body = prog in
	let _ = type_instr env main_body in ()