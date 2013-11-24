open Ast
open Mips

let caller_method label_name =
	let push_treg = push t0 @@ push t1 @@push t2 @@ push t3 in
	let pop_treg = pop t3 @@ pop t2 @@ pop t1 @@ pop t0 in
	push_treg @@ jal label_name @@ pop_treg
	
let callee_method loc_size code =
	let init = comment "init" @@ push fp @@ push ra in
	let init_fp = sub fp sp oi loc_size @@ move sp fp in
	let final = comment "final" @@ add sp fp oi loc_size @@ pop ra @@ pop fp @@ jr ra
	in init @@ init_fp @@ comment "code" @@ code @@ final
	
let print_str label_name =
	la t0 alab label_name
	@@ caller_method "print_string"	

(* LISTE DES GENERATEURS D'ETIQUETTE *)
let next = let r = ref 0 in fun () -> r:= !r +1; !r
let cond i = Printf.sprintf "cond%i" i
let endcond i = Printf.sprintf "endcond%i" i
(* ######################################################## *)

(* LISTE DES ERREURS D'EXECUTION *)
let err_div_by_zero = label "err_div_by_zero" @@ asciiz "division par zéro"
(* ######################################################## *)

(* LISTE DES DATAS PAR DEFAUTS *)
let btrue = label "btrue" @@ asciiz "true"
let bfalse = label "bfalse" @@ asciiz "false"
let backslashn = label "backslashn" @@ asciiz "\r\n"
(* ######################################################## *)

(* LISTE DES LABELS DE DATA *)
let data_label = btrue @@ bfalse @@ backslashn @@ err_div_by_zero

(* ######################################################## *)

(* LISTE DES APPELS LORS D'ERREURS *)
let raise_error error_name = print_str error_name @@ b "end"
let cerr_div_by_zero = label "cerr_div_by_zero" @@ raise_error "err_div_by_zero"
(* ######################################################## *)

module Env = Map.Make(String)

let end_ = label "end"

let print i =
	add a0 t0 oi 0
	@@ li v0 i
	@@ syscall

let print_int =
	label "print_int" @@ callee_method 0 (print 1)

let print_string =
	label "print_string" @@ callee_method 0 (print 4)

(* code qui effectue un if then else et qui branche sur code 1 ou code 2 *)
let compile_cond code1 code2 =
	let i = next() in
	beqz t0 (cond i) @@ code1 @@ b (endcond i) @@ label (cond i) @@ code2 @@ label (endcond i)

(* code qui effectue un for et qui branche sur code 1 *)
let compile_for e1' e2' e3' code1 =
	let i = next() in
	e1' @@ label(cond i) @@ e2' @@ beqz t0 (endcond i) @@ code1 @@ e3' @@ b (cond i) @@ label (endcond i)

let rec compile_expr loc_size env e =
	match e.node with
	| Econst (c) -> let t =
				match c with
					Cint v32 -> li32 t0 v32
				| Cstring _ -> assert false
				| Cbool vbool -> if vbool then li t0 1 else li t0 0
				| Cnull -> li t0 0
			in t
  | Elval l -> let clval = compile_lval_access loc_size env l in
               clval
  | Eassign (l, e) ->
      let cexp = compile_expr loc_size env e in
      let cl = compile_lval_assign loc_size env t0 l in
			cexp @@ cl	
	| Ebinop (e1, o, e2) ->
			(match o with
				| Beq -> compile_binop loc_size env e1 e2 @@ seq t0 t1 t0
				| Bneq -> compile_binop loc_size env e1 e2 @@ sne t0 t1 t0
				| Blt -> compile_binop loc_size env e1 e2 @@ slt t0 t1 t0
				| Blte -> compile_binop loc_size env e1 e2 @@ sle t0 t1 t0
				| Bgt -> compile_binop loc_size env e1 e2 @@ sgt t0 t1 t0
				| Bgte -> compile_binop loc_size env e1 e2 @@ sge t0 t1 t0
				| Band -> compile_binop loc_size env e1 e2 @@ and_ t0 t1 t0
				| Bor -> compile_binop loc_size env e1 e2 @@ or_ t0 t1 t0
				| Badd -> compile_binop loc_size env e1 e2 @@ add t0 t1 oreg t0
				| Bsub -> compile_binop loc_size env e1 e2 @@ sub t0 t1 oreg t0
				| Bmul -> compile_binop loc_size env e1 e2 @@ mul t0 t1 oreg t0
				| Bdiv -> compile_binop loc_size env e1 e2 @@ beqz t0 "cerr_div_by_zero" @@ div t0 t1 oreg t0
				| Bmod -> compile_binop loc_size env e1 e2 @@ beqz t0 "cerr_div_by_zero" @@ rem t0 t1 oreg t0
			)
	| Eunop (unop, e) ->
			(	
				match unop,e.node with
				| Unot,_ -> compile_expr loc_size env e @@ compile_cond (sub t0 t0 oreg t0) (add t0 t0 oi 1)
				| Uneg,_ -> compile_expr loc_size env e @@ neg t0 t0
				| Upost_inc,Elval l -> compile_expr loc_size env e @@ add t1 t0 oi 1 @@ compile_lval_assign loc_size env t1 l
				| Upost_dec,Elval l -> compile_expr loc_size env e @@ sub t1 t0 oi 1 @@ compile_lval_assign loc_size env t1 l
				| Upre_inc,Elval l ->  compile_expr loc_size env e @@ add t0 t0 oi 1 @@ compile_lval_assign loc_size env t0 l
				| Upre_dec,Elval l ->  compile_expr loc_size env e @@ sub t0 t0 oi 1 @@ compile_lval_assign loc_size env t0 l
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
							| Tint -> cexpr @@ caller_method "print_int" @@ print_str "backslashn"
							| Tboolean ->
									let code1 = print_str "btrue" @@ print_str "backslashn" in
									let code2 = print_str "bfalse" @@ print_str "backslashn" in
									cexpr @@ compile_cond code1 code2
							| Tclass "String" -> assert false
							| _ -> assert false
						else assert false
				| _ -> assert false)
	| _ -> assert false

and compile_binop loc_size env e1 e2 =
	compile_expr loc_size env e1 @@ push t0 @@ compile_expr loc_size env e2 @@ pop t1
	
and compile_lval_access loc_size env l =
  match l with
    Lident x -> begin
      try
        let fp_shift = Env.find x.node env in
        lw t0 areg (fp_shift,fp)
      with Not_found -> assert false
    end
  | Laccess (e, x) -> assert false

and compile_lval_assign loc_size env reg l =
  match l with
    Lident x -> begin
      try
        let fp_shift = Env.find x.node env in
        sw reg areg (fp_shift,fp)
      with Not_found -> assert false
    end
  | Laccess (e, x) -> assert false

(* [compile_opt env oe] génère le code d'une expression contenue dans un      *)
(* option                                                                  *)
let compile_opt loc_size env oe =
	match oe with
		None -> move t0 zero
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
	| Idecl (t, x, eopt) -> (match t with
													| Tint | Tboolean -> 
														let shift = fp_shift + 4 in
														let new_env = Env.add x.node shift env in
														shift,loc_size,new_env,compile_opt loc_size new_env eopt @@ sw t0 areg(shift,fp)
													| _ -> assert false	 )
	| Iif (e, i1, i2) ->
			let cexpr = compile_expr loc_size env e in
			let _,_,_,code1 = compile_instr fp_shift loc_size env i1
			and _,_,_,code2 = compile_instr fp_shift loc_size env i2 in
			fp_shift, loc_size, env, cexpr @@ compile_cond code1 code2
	| Ifor (oe1, oe2, oe3, i') ->
			(match oe1, oe2, oe3 with
				| Some e1, Some e2, Some e3 ->
						let e1' = compile_expr loc_size env e1 in
						let e2' = compile_expr loc_size env e2 in
						let e3' = compile_expr loc_size env e3 in
						let _,_,_,code1 = compile_instr fp_shift loc_size env i' in
						fp_shift, loc_size, env, compile_for e1' e2' e3' code1
				| _ -> assert false)
	| Iblock li ->
			let rec aux cfp_shift cloc_size cenv clist =
				match clist with
				| [] -> cfp_shift, cloc_size, cenv, nop
				| a::r -> let new_fpshift,_,new_env,cins = compile_instr cfp_shift loc_size cenv a in
									let a,b,c,d = aux new_fpshift cloc_size new_env r in
									a,b,c,cins @@ d
			in aux fp_shift loc_size env li
	| _ -> fp_shift, loc_size, env, comment "c'est le main"

let prog (class_list, main_class, main_body) =
	let loc_size = get_local_size main_body in
	let fp_shift,_,_,body_code = compile_instr (-4) loc_size Env.empty main_body in
	{
		text =
			caller_method "main"
			@@ b "end"
			@@ label "main"
			@@ comment "c'est le main"
			@@ callee_method loc_size body_code
			@@ print_int
			@@ print_string
			@@ cerr_div_by_zero
			@@ end_;
		data = data_label;
	}