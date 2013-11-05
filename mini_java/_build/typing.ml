open Ast
open Type_class


type t = string
let stringOf t = t
let fromString t = t

module Env = Map.Make(String)
let env = Env.empty

let env_fun = Env.empty
let env_glob = Env.empty

let rec subtype t1 t2 = 
		match  t1,t2  with
		| Tvoid,Tvoid -> true
		| Tboolean, Tboolean -> true 
		| Tint, Tint -> true
		| Tnull, Tnull -> true
		| (Tint|Tclass "String"), (Tclass "String"|Tint) -> true
		| Tclass s1, Tclass s2 -> if s1 = s2 then true else false
		| Tnull, Tclass _ -> true
		| _,_ -> false

let add_node i n = {node = n ; info = i }

let compatible t1 t2 = subtype t1 t2 || subtype t2 t1



(*let rec is_lval e = 
	match e.node with
		|Lident _ -> true
		|Laccess (id,_) -> is_lval id 
		|_ -> false*)

let is_string t =
	match t with
	|Tclass "String" -> true
	|_ -> false


let findClass env c  = Env.mem c env
let isClass env c = 
	try
		findClass env c 
	with
	| Not_found-> false


(*typage d'une expression pour chaque expression, on commence par typer les sous-expression
 pour typer l'expression de resultat*)
let rec type_expr env e = 
	match e.node with
	|Econst c -> let out_type = 
		       match c with
				|Cint c ->
				  Tint
				|Cstring s -> 
				  Tclass "String"
				|Cbool b -> 
				  Tboolean
				|Cnull -> 
				  Tnull
		     in add_node out_type (Econst c)

	(*|Elval e ->
		match e with 
		|Lident id ->
			let out_type = 
			try
				Env.find id.node env
			with
			 Not_found -> failwith "Unknown"
		|Laccess(e0,id) ->	
			*)

	(*|Eassign (l,e) -> 

			let type_var = type_expr env l in
			if !(is_lval type_Var) 
				then 
					failwith "type_var should be a  lvar"
			else
				let t2 = type_expr env e in
				if compatible type_expr.info t2.info 
					then
						add_node type_expr.info (Eassign (type_var, t2))

	|Ecall (l,elist) ->
		let ret_typ param_list = 
		try 
			Env.find l.node fun_env
		with 
			Not_found -> failwith "type call inconnue"
			in 
			begin
		try
			let args0 = List.map2
			(fun e0 (tx,_) ->
			let et0 = type_expr env e0 in
			if compatible et0.node tx 
			then 
				et0 
			else 
				failwith "type Ecall error"
			)args param_list
		in
			add_node ret_typ (Ecall (l,args0))
		with
		 	Invalide_argument _ -> failwith "function %s expects %i arguments but was called with %i"
		end
	(*|Enew (id,elist) ->*) *)
	|Eunop(op,e) -> 
		let te = type_expr env e
		in
		let out_type = 
		  match op with
		  |Upost_inc | Upost_dec
		  |Upre_inc | Upre_dec -> 
			if compatible te.info Tint
			then
			  te.info
			else 
			  failwith "invalie type Eunop"
		|Unot ->
			if compatible te.info Tboolean
			then
			  te.info
			else 
				failwith "type Unot error"
		|Uneg -> 
			if compatible te.info Tint
			then 
				te.info
			else 
				failwith "type Uneg error"
		in
		add_node out_type (Eunop(op,te))

	|Ebinop (e1,op,e2) -> 
		let t1 = type_expr env e1
		and t2 = type_expr env e2
		in
		begin
		  match op with
		  |Band | Bor -> 
		    if compatible t1.info Tboolean && compatible t2.info Tboolean
		    then
		      add_node Tboolean (Ebinop(t1,op,t2))
		    else 
		      failwith "type bool error"		
		  |Beq | Bneq -> 
		    if compatible t1.info t2.info 
		    then
		      add_node t1.info (Ebinop(t1,op,t2))
		    else
		      failwith "error beq | bneq error"
		  |Blt |Blte | Bgt | Bgte ->
		    if compatible t1.info Tint && compatible t2.info Tint
		    then
		      add_node Tint (Ebinop(t1,op,t2))
		    else
		      failwith "error Blt | Blte | Bgt | Bgte"	
		  | Bsub | Bmul | Bdiv | Bmod ->
		    if compatible t1.info Tint && compatible t2.info Tint
		    then 
		      add_node Tint (Ebinop(t1,op,t2))	
		    else 
		      failwith "error Sbub ... type"
		  |Badd -> 
		    if compatible t1.info (Tclass "String" ) 
		    then if compatible t2.info Tint || compatible t2.info (Tclass "String")
		      then 
			add_node (Tclass "String") (Ebinop(t1,op,t2))
		      else failwith "type String add error"
		    else if 
			compatible t2.info (Tclass "String")
		    then if compatible t1.info Tint || compatible t1.info (Tclass "String")
		      then 
			add_node (Tclass "String") (Ebinop (t1,op,t2))
		      else 
			failwith "type String add error"
		    else if compatible t1.info Tint && compatible t2.info Tint
		    then 
		      add_node Tint (Ebinop (t1,op,t2))
		    else failwith "type Int add error"
		end
	(*|Einstanceof (e,t) ->
	  let te = type_expr env e in
	  if (te.info = Tnull || isClass te)
	  then
	    if compatible te.info t then 
	      add_node Tboolean (Einstanceof (te,t))
	    else 
	      failwith "type error einstanceof"
	  else
	    failwith " te should be a class or null"
	|Ecast (t,e) -> 
	  if (not(Env.mem (stringOf t) env))
		then
			failwith "t should be a type Tclass --Ecast"
		else
			let te = type_expr env e 
			in
			if ((subtype t te.info)|| (subtype te.info t))
			then 
			  add_node te.info (Ecast(t,te))
		else 
			  failwith "typing error Ecast "
*)




let rec type_instr env i =   (* : type_instr Env.empty main_body *)
	let instr_node = i.node in
		match instr_node with
		|Iexpr e -> let te = type_expr env e 
					in 
			    add_node te.info (Iexpr e)
		(* |Idecl(t,id,None) ->  ()  *)
		(* |Idecl(t,id,Some e) -> () *)
		(* |Iif (e1,in1,in2) ->                      *)
		(* 	let t1 = type_expr env e1 in            *)
		(* 		if t1.info <> Tboolean                *)
		(* 		then failwith "type error if"         *)
		(* 		else                                  *)
		(* 		  let tin1 = type_instr env in1       *)
		(* 		  and tin2 = type_instr env in2       *)
		(* 	in                                      *)
		(* 		  add_node Tvoid (Iif (t1,tin1,tin2)) *)
		(*|Ifor (e1,e2,e3,in1) -> 
			let _,te1 = type_block env ret [] e1 in
			let te2 = type_expr env e2 in
			if not(compatible ct0.info Tint )
			then
				failwith "type for error"
			let _,te3 = type_block env ret [] e3 in
			let tin1 = type_instr env b in
			add_node Tvoid (Ifor(te1,te2,te3,tin1))
		|Iblock(in_list)
		|Ireturn
		|_ -> failwith "todo"

and type_block env ret il sl = 
	let il0, env0 = type decl env il in
	let sl0 = List.map(type_instr env ret) sl in
	il0, sl0*)

let type_prog prog = 	
	init_table prog;
	let clist, name ,main_body = prog in
	(* type_instr Env.empty main_body; *)
	()