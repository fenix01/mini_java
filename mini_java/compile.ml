open Ast
open Mips
open Descriptor
open Runtime

module Env = Map.Make(String)
let tbl_cstr = Hashtbl.create 100

let compile_tbl_cstr () =
	Hashtbl.fold (
		fun id value acc ->
			acc @@ label id @@ asciiz value
		) tbl_cstr nop 

let enter_params l env loc_size =
	let start_shift = loc_size + 8 in
	let tenv = Env.add "this" start_shift env in
	let genv,shift = List.fold_left ( fun (new_env,shift) (_,name_) ->
		(Env.add name_.node (shift+4) new_env),shift+4
	) (tenv,start_shift) l in
	genv
	
let print_str label_name =
	la t0 alab label_name
	@@ caller_lmethod "print_string" nop 0

(* LISTE DES ERREURS D'EXECUTION *)
let err_div_by_zero = label "err_div_by_zero" @@ asciiz "division by zero"
let err_null_pointer = label "err_null_pointer" @@ asciiz "null pointer exception"
(* ######################################################## *)

(* LISTE DES DATAS PAR DEFAUTS *)
let btrue = label "btrue" @@ asciiz "true"
let bfalse = label "bfalse" @@ asciiz "false"
let backslashn = label "backslashn" @@ asciiz "\r\n"
(* ######################################################## *)

(* LISTE DES LABELS DE DATA *)
let data_label = btrue @@ bfalse @@ backslashn @@ err_div_by_zero @@ err_null_pointer

(* ######################################################## *)

(* LISTE DES APPELS LORS D'ERREURS *)
let raise_error error_name = print_str error_name @@ b "end"
let cerr_div_by_zero = label "cerr_div_by_zero" @@ raise_error "err_div_by_zero"
let cerr_null_pointer = label "cerr_null_pointer" @@ raise_error "err_null_pointer"
let cerrors = cerr_div_by_zero @@ cerr_null_pointer
(* ######################################################## *)

let end_ = label "end"

let print i =
	add a0 t0 oi 0
	@@ li v0 i
	@@ syscall

let print_int =
	label "print_int" @@ callee_method 0 (print 1)

let print_string =
	label "print_string" @@ callee_method 0 (print 4)

let rec compile_expr loc_size env e =
	match e.node with
	(* stocke les valeurs dans le registre t0 *)
	| Econst (c) -> let t =
				match c with
					Cint v32 -> li32 t0 v32
				| Cstring vstr -> 
					let vstr_length = Int32.of_int (String.length vstr) in
					let label_str = next_str() in 
					Hashtbl.add tbl_cstr label_str vstr;
					let this_addr = get_this_addr "String" in	
					let c_desc = class_desc "String" in 
					alloc_mem c_desc this_addr.attrs_shift @@
					pushad @@ 
					la t1 alab(label_str) @@ 
					li32 t2 vstr_length @@
					sw t1 areg (4,t0) @@
					sw t2 areg (8,t0) @@
					popad
				| Cbool vbool -> if vbool then li t0 1 else li t0 0
				| Cnull -> li t0 0
			in t
	(* utilise les registres t0 et t1 *)
  | Elval l -> let clval = compile_lval loc_size false env l in
               clval
	(* utilise les registres t0 et t1 *)
  | Eassign (l, e) ->
      let cexp = compile_expr loc_size env e in
      let cl = compile_lval loc_size true env l in
			cexp @@ cl	
	(* utilise les registres t0 et t1 *)
	| Ebinop (e1, o, e2) ->
			(match o with
				| Beq -> compile_binop loc_size env e1 e2 @@ seq t0 t1 t0
				| Bneq -> compile_binop loc_size env e1 e2 @@ sne t0 t1 t0
				| Blt -> compile_binop loc_size env e1 e2 @@ slt t0 t1 t0
				| Blte -> compile_binop loc_size env e1 e2 @@ sle t0 t1 t0
				| Bgt -> compile_binop loc_size env e1 e2 @@ sgt t0 t1 t0
				| Bgte -> compile_binop loc_size env e1 e2 @@ sge t0 t1 t0
				| Band -> 
						compile_expr loc_size env e1 @@
						compile_cond  
						(push t0 @@ 
						compile_expr loc_size env e2 @@ 
						pop t1 @@
						and_ t0 t1 t0)
						nop
				| Bor -> 
						compile_expr loc_size env e1 @@
						compile_cond  
						(push t0 @@ 
						compile_expr loc_size env e2 @@ 
						pop t1 @@
						and_ t0 t1 t0)
						nop
				| Badd -> compile_add loc_size env e1 e2
				| Bsub -> compile_binop loc_size env e1 e2 @@ sub t0 t1 oreg t0
				| Bmul -> compile_binop loc_size env e1 e2 @@ mul t0 t1 oreg t0
				| Bdiv -> compile_binop loc_size env e1 e2 @@ compile_cond (div t0 t1 oreg t0) (b "cerr_div_by_zero")
				| Bmod -> compile_binop loc_size env e1 e2 @@ beqz t0 "cerr_div_by_zero" @@ rem t0 t1 oreg t0
			)
	(* utilise les registres t0 et t1 *)
	| Eunop (unop, e) -> 
			(	
				match unop,e.node with
				| Unot,_ -> compile_expr loc_size env e @@ compile_cond (sub t0 t0 oreg t0) (add t0 t0 oi 1)
				| Uneg,_ -> compile_expr loc_size env e @@ neg t0 t0
				| Upost_inc,Elval l -> compile_expr loc_size env e @@ add t1 t0 oi 1
															@@ switch t0 t1 @@ compile_lval loc_size true env l @@ switch t1 t0
				| Upost_dec,Elval l -> compile_expr loc_size env e @@ sub t1 t0 oi 1
															@@ switch t0 t1 @@ compile_lval loc_size true env l @@ switch t1 t0
				| Upre_inc,Elval l ->  compile_expr loc_size env e @@ add t0 t0 oi 1 @@ compile_lval loc_size true env l
				| Upre_dec,Elval l ->  compile_expr loc_size env e @@ sub t0 t0 oi 1 @@ compile_lval loc_size true env l
				| _,_ -> assert false
			)
	|	Ecall (lval, args) ->
			(match lval with
				| Lident f ->
						let comp = String.compare f.node "System$out$print" in
						if comp = 0 then
							let expr_ = List.hd args in
							let cexpr = compile_expr loc_size env expr_ in
							match expr_.info with
							| Tint -> cexpr @@ caller_lmethod "print_int" nop 0
							| Tboolean ->
									let code1 = print_str "btrue" in
									let code2 = print_str "bfalse" in
									cexpr @@ compile_cond code1 code2
							| Tclass "String" -> 
								cexpr @@ 
								lw t0 areg (4,t0) @@ 
								caller_lmethod "print_string" nop 0
							| _ -> exit 0
						else exit 0
				| Laccess (e,x) -> let cexpr = compile_expr loc_size env e in
													 let class_name = 
													 match e.info with
													 | Tclass cname -> cname
													 | _ -> ""
													 in
													 let this_addr = get_this_addr class_name in
													 cexpr @@ call_method this_addr x.node args env loc_size
				)
	  | Enew (cls, args) -> let class_name = 
													match cls.info with
													| Tclass cname -> cname
													| _ -> ""
													in
													let this_addr = get_this_addr class_name in	
													let c_desc = class_desc class_name in
													alloc_mem c_desc this_addr.attrs_shift 
													@@ call_method this_addr cls.node args env loc_size
	| Ecast (_,_) -> Printf.printf "cast not implemented"; exit 2
	| Einstanceof (_,_) -> Printf.printf "dynamic instanceof not implemented"; exit 2

and compile_binop loc_size env e1 e2 =
	comment "compile_binop" @@ 
	compile_expr loc_size env e1 @@ 
	push t0 @@ 
	compile_expr loc_size env e2 @@ 
	pop t1
	
and compile_add loc_size env e1 e2 =
	match e1.info,e2.info with
	| Tclass "String",Tclass "String" -> 
		compile_binop loc_size env e1 e2 @@ 
		caller_lmethod "concatenate_str" nop 0 @@
		move t0 v1
	| Tclass "String", Tint -> Printf.printf "convertion not implemented";exit 2
	| Tint, Tclass "String" -> Printf.printf "convertion not implemented";exit 2
	| _,_ -> compile_binop loc_size env e1 e2 @@ add t0 t1 oreg t0
	
and compile_args loc_size env args =
	let cargs,size = List.fold_left (
		fun (acc, shift) x ->
			let cexp = compile_expr loc_size env x in
			acc @@ cexp @@ push t0,shift+4
    ) (nop, 0) (List.rev args)
	in 
	comment "prepare args" @@
	move t3 t0 @@
	cargs @@
	push v0 @@
	move t0 t3 @@
	comment "end args",size
	
and call_method this_addr method_name args env loc_size =
	(* let meth_shift = get_method_addr this_addr.methods_desc method_name in *)
	let meth_shift = get_shift method_name in
	let descriptor = lw t0 areg (0,t0) in
	let meth = lw t0 areg (meth_shift,t0) in
	let cargs,size = compile_args loc_size env args in
	pushad @@
	descriptor @@
	meth @@
	caller_rmethod t0 cargs (size+4) @@
	popad @@
	move t0 v1

(* rw = lecture = false, ecriture = true*)
(* reg = registre servant pour la lecture ou l'écriture *)	
and compile_lval loc_size rw env l =
  match l with
    Lident x -> 
      		(try
        		let fp_shift = Env.find x.node env in
        		if rw then sw t0 areg (fp_shift,fp) else lw t0 areg (fp_shift,fp)
      		with Not_found -> assert false)
  | Laccess (e, x) -> 
					(try
						let class_name = 
							match e.info with
							| Tclass cname -> cname
							| _ -> ""
						in
						let class_addr = get_this_addr class_name in
						let cexp = compile_expr loc_size env e in
						let attr_shift = get_attr_addr class_addr.attrs_desc x.node in
        		if rw then 
							push t0 @@ cexp @@ pop t1 @@ compile_cond (sw t1 areg (attr_shift,t0)) (b "cerr_null_pointer")
									else
							push t0 @@ cexp @@ pop t1 @@ compile_cond (lw t0 areg (attr_shift,t0)) (b "cerr_null_pointer")
      		with Not_found -> assert false)

(* [compile_opt env oe] génère le code d'une expression contenue dans un      *)
(* option                                                                  *)
let compile_opt loc_size env oe =
	match oe with
		None -> nop
	| Some e -> compile_expr loc_size env e


let rec get_local_size linstr =
	match linstr.node with
	| Idecl (_, _, _) -> 4
	| Iif (_, i1, i2) -> get_local_size i1 + get_local_size i2
	| Ifor (_, _, _, i') -> get_local_size i'
	| Iblock li -> let rec aux lt =
									match lt with
									| [] -> 0
									| a :: r -> get_local_size a + aux r
									in aux li
	| _ -> 0

let rec compile_instr fp_shift loc_size env instr =
	match instr.node with
	| Iexpr e -> fp_shift, loc_size, env, compile_expr loc_size env e
	| Idecl (t, x, eopt) ->		let shift = fp_shift + 4 in
														let new_env = Env.add x.node shift env in
														shift,loc_size,new_env,
														move t0 zero @@
														compile_opt loc_size new_env eopt @@
														sw t0 areg(shift,fp)
	| Iif (e, i1, i2) ->
			let cexpr = compile_expr loc_size env e in
			let _,_,_,code1 = compile_instr fp_shift loc_size env i1
			and _,_,_,code2 = compile_instr fp_shift loc_size env i2 in
			fp_shift, loc_size, env, cexpr @@ compile_cond code1 code2
	| Ifor (oe1, oe2, oe3, i') ->
						let e1' = compile_opt loc_size env oe1 in
						let e2' = compile_opt loc_size env oe2 in
						let e3' = compile_opt loc_size env oe3 in
						let _,_,_,code1 = compile_instr fp_shift loc_size env i' in
						fp_shift, loc_size, env, compile_for e1' e2' e3' code1
	| Iblock li ->
			let rec aux cfp_shift cloc_size cenv clist =
				match clist with
				| [] -> cfp_shift, cloc_size, cenv, nop
				| a::r -> let new_fpshift,_,new_env,cins = compile_instr cfp_shift loc_size cenv a in
									let a,b,c,d = aux new_fpshift cloc_size new_env r in
									a,b,c,cins @@ d
			in aux fp_shift loc_size env li
  | Ireturn oe ->
      let coe = compile_opt loc_size env oe in
      fp_shift, loc_size, env,comment "return" @@ coe @@ move v0 t0 @@ hack_return loc_size

let rem_assoc l =
	let rec rm_l  l' = 
		match l' with
		| [] -> []
		| a::r -> let u,v = a in u::rm_l r
	in rm_l l

let rec compile_class this_addr defns =
	match defns with
	| [] -> nop
	| def :: r ->
		let cmethod = match def with
							| Dconstr (f, params, i ) ->
								let desc_name =  "_ctor$" ^ method_desc this_addr.name_ f.node (rem_assoc params) in
								let ctor_addr = get_method_addr this_addr.methods_desc desc_name in
								let loc_size = get_local_size i in
								let env = enter_params params Env.empty loc_size in
								let _,_,_,cctor_b = compile_instr (-4) loc_size env i in
								let cctor = callee_method loc_size cctor_b in
								label desc_name @@ (comment (Printf.sprintf "%i" ctor_addr)) @@ cctor
							| Dmeth (ret, f, params, i ) ->
								let desc_name = "_meth$" ^ method_desc this_addr.name_ f.node (rem_assoc params) in
								let meth_addr = get_method_addr this_addr.methods_desc desc_name in
								let loc_size = get_local_size i in
								let env = enter_params params Env.empty loc_size in
								let _,_,_,cmeth_b = compile_instr (-4) loc_size env i in
								let cmeth = callee_method loc_size cmeth_b in
								label desc_name @@ (comment (Printf.sprintf "%i" meth_addr)) @@ cmeth
							| _ -> nop
		in cmethod @@ compile_class this_addr r

let compile_classes class_list =
		let rec compile clist =
		match clist with
		| [] -> nop
		| (this, parent, defns) :: r ->
			let this_addr = get_this_addr this.node in
			compile_class this_addr defns @@ compile r
		in compile class_list

let prog (class_list, main_class, main_body) =
	build_descriptors();
	let cclasses = compile_classes class_list in
	let loc_size = get_local_size main_body in
	let fp_shift,_,_,body_code = compile_instr (-4) loc_size Env.empty main_body in
	let c_str = compile_tbl_cstr () in
	{
		text =
			caller_lmethod "main" nop 0
			@@ b "end"
			@@ label "main"
			@@ comment "c'est le main"
			@@ callee_method loc_size body_code
			@@ is_equal
			(* @@ count_bytes *)
			@@ concatenate_string
			@@ print_int
			@@ print_string
			@@ cerrors
			@@ cclasses
			@@ end_;
		data = 
			classes_addr.descriptors
			@@ data_label @@ c_str;
	}