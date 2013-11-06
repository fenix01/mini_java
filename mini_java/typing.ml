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
	| Tnull, Tclass _ -> true
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
			(* (match e with                                                          *)
			(* 	| Lident id ->                                                       *)
			(* 			(try                                                             *)
			(* 				let type_ = Env.find id.node env in                            *)
			(* 				type_                                                          *)
			(* 			with                                                             *)
			(* 				Not_found -> failwith "erreur critique, variable non trouvée") *)
			(* 	| Laccess (_, _) ->	failwith "Unknown"                              *)
			(* )                                                                      *)
	| Eassign (l, exp) ->
	(* let type_var = type_lvalue env l in let t2 = type_expr env exp  *)
	(* in if compatible type_var t2.info then add_node t2.info         *)
	(* (Eassign (l, t2)) else failwith ""                              *)
			failwith "todo"
	| Ecall (l, elist) ->
	(* let ret_typ param_list = try Env.find l.node fun_env with Not_found *)
	(* -> failwith "type call inconnue" in begin try let args0 = List.map2 *)
	(* (fun e0 (tx,_) -> let et0 = type_expr env e0 in if compatible       *)
	(* et0.node tx then et0 else failwith "type Ecall error" )args         *)
	(* param_list in add_node ret_typ (Ecall (l,args0)) with               *)
	(* Invalide_argument _ -> failwith "function %s expects %i arguments   *)
	(* but was called with %i" end                                         *)
			failwith "todo"
	| Enew (id, elist) -> failwith "todo"
	| Eunop(op, e) -> failwith "todo"
	(* let te = type_expr env e in let out_type = match op with |Upost_inc | *)
	(* Upost_dec |Upre_inc | Upre_dec -> if compatible te.info Tint then     *)
	(* te.info else failwith "invalie type Eunop" |Unot -> if compatible     *)
	(* te.info Tboolean then te.info else failwith "type Unot error" |Uneg   *)
	(* -> if compatible te.info Tint then te.info else failwith "type Uneg   *)
	(* error" in add_node out_type (Eunop(op,te))                            *)
	| Ebinop (e1, op, e2) ->
			let t1 = type_expr env e1 in
			let t2 = type_expr env e2 in
			(
				print t1;
				print t2;
				match op with
				| Band | Bor | Beq | Bneq | Blt | Blte | Bgt | Bgte ->
						(if compatible t1 t2 then
								Tboolean
							else
								raise (Expression_error "les types comparés sont incompatibles"))
				| _ -> raise (Expression_error "erreur d'expression inconnu")
			)
	(* |Beq | Bneq -> if compatible t1.info t2.info then add_node t1.info  *)
	(* (Ebinop(t1,op,t2)) else failwith "error beq | bneq error" |Blt      *)
	(* |Blte | Bgt | Bgte -> if compatible t1.info Tint && compatible      *)
	(* t2.info Tint then add_node Tint (Ebinop(t1,op,t2)) else failwith    *)
	(* "error Blt | Blte | Bgt | Bgte" | Bsub | Bmul | Bdiv | Bmod -> if   *)
	(* compatible t1.info Tint && compatible t2.info Tint then add_node    *)
	(* Tint (Ebinop(t1,op,t2)) else failwith "error Sbub ... type" |Badd   *)
	(* -> if compatible t1.info (Tclass "String" ) then if compatible      *)
	(* t2.info Tint || compatible t2.info (Tclass "String") then add_node  *)
	(* (Tclass "String") (Ebinop(t1,op,t2)) else failwith "type String add *)
	(* error" else if compatible t2.info (Tclass "String") then if         *)
	(* compatible t1.info Tint || compatible t1.info (Tclass "String")     *)
	(* then add_node (Tclass "String") (Ebinop (t1,op,t2)) else failwith   *)
	(* "type String add error" else if compatible t1.info Tint &&          *)
	(* compatible t2.info Tint then add_node Tint (Ebinop (t1,op,t2)) else *)
	(* failwith "type Int add error" end                                   *)
	| Einstanceof (e, t) -> failwith "todo"
	(* let te = type_expr env e in if (te.info = Tnull || isClass te) then *)
	(* if compatible te.info t then add_node Tboolean (Einstanceof (te,t)) *)
	(* else failwith "type error einstanceof" else failwith " te should be *)
	(* a class or null"                                                    *)
	| Ecast (t, e) -> failwith "todo"
(* if (not(Env.mem (stringOf t) env)) then failwith "t should be a type  *)
(* Tclass --Ecast" else let te = type_expr env e in if ((subtype t       *)
(* te.info)|| (subtype te.info t)) then add_node te.info (Ecast(t,te))   *)
(* else failwith "typing error Ecast "                                   *)

and type_lvalue env l =
	match l with
	| Lident x -> (try Env.find x.node env 
	with Not_found -> raise (Expression_error ("la variable" ^ x.node ^ " n'existe pas") ))
	| Laccess (e, x) -> let t = type_expr env e in
			match t with
			| _ -> failwith "..."
			| _ -> Tnull

let rec type_instr env i =   (* : type_instr Env.empty main_body *)
	match i.node with
	| Iexpr e -> let _ = type_expr env e
			in
			env
	| Idecl(t, id, None) -> if Env.mem id.node env then
				error ("la variable " ^ id.node ^ " est déjà défini") id.info
			else
				Env.add id.node t env
	| Idecl(t, id, Some e) -> if Env.mem id.node env then
				error ("la variable " ^ id.node ^ " est déjà défini") id.info
			else
				(let type_ = type_expr env e in
					let check = compatible type_ t in
					if not check then
						error ("la variable " ^ id.node ^ " est mal typée") id.info
					else Env.add id.node t env)
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
	| Ifor (Some e1, Some e2, Some e3, in1) ->
	(* let _,te1 = type_block env [] e1 in *)
			let te2 = type_expr env e2 in
			if not(compatible te2 Tint )
			then
				failwith "type for error";

			let _ = type_instr env in1 in
			env
	| Iblock(in_list) -> 	
		let rec aux env list =
				match list with
				| [] -> env
				| a :: r -> let new_env = type_instr env a in aux new_env r
			in aux env in_list
	| Ireturn _ -> failwith "todo2"
	| _ -> failwith "le compilateur a rencontré une erreur inattendue"

(* and type_block env il sl = (* let il0, env0 = type_decl env il in *)    *)
(* let sl0 = List.map(type_instr env) sl in il0, sl0                       *)

let type_prog prog =
	init_table prog;
	let clist, name , main_body = prog in
	let _ = type_instr env main_body in ()