open Ast
open Mips

let print_str label_name =
	la t0 alab label_name
	@@ jal "print_string"	
	
(*LISTE DES GENERATEURS D'ETIQUETTE*)
let next = let r = ref 0 in fun () -> r:= !r+1; !r
let cond i = Printf.sprintf "cond%i" i
let endcond i = Printf.sprintf "endcond%i" i
(*########################################################*)

(*LISTE DES ERREURS D'EXECUTION*)
let err_div_by_zero = label "err_div_by_zero" @@ asciiz "division par zéro"
(*########################################################*)

(*LISTE DES DATAS PAR DEFAUTS*)
let btrue = label "btrue" @@ asciiz "true"
let bfalse = label "bfalse" @@ asciiz "false"
let backslashn = label "backslashn" @@ asciiz "\r\n"
(*########################################################*)

(*LISTE DES LABELS DE DATA*)
let data_label = btrue @@ bfalse @@ backslashn @@ err_div_by_zero

(*########################################################*)

(*LISTE DES APPELS LORS D'ERREURS*)
let raise_error error_name = print_str error_name @@ b "end"
let cerr_div_by_zero = label "cerr_div_by_zero" @@ raise_error "err_div_by_zero"
(*########################################################*)

let end_ = label "end"

let print i =
		add a0 t0 oi 0
	@@ li v0 i
	@@ syscall
	@@ jr ra
	
let print_int =
	label "print_int" @@ print 1
	
let print_string =
	label "print_string" @@ print 4

let exit_ =
	label "exit" @@ print 10
								
(* code qui effectue un if then else et qui branche sur code 1 ou code 2 *)
let compile_cond code1 code2 =
	let i = next() in
	beqz t0 (cond i) @@ code1 @@ b (endcond i) @@ label (cond i) @@ code2 @@ label (endcond i)
	
(* code qui effectue un for et qui branche sur code 1 *)
let compile_for finit fcond fstate code1 =
	let prepare = finit @@ fstate @@ fcond  in
	let i = next() in
	prepare @@ label(cond i) @@ beqz t0 (endcond i) @@ code1 @@ add t2 t2 oreg t1 @@ b (cond i) @@ label (endcond i)
														
let rec compile_expr env e =
  match e.node with
	| Econst (c) -> let t =
                    match c with
                      Cint v32   ->  li32 t0 v32
                    | Cstring _ -> assert false
                    | Cbool vbool -> if vbool then li t0 1 else li t0 0
                    | Cnull -> li t0 0
									in t
	| Ebinop (e1, o, e2) ->
        (match o with
        | Beq -> compile_binop env e1 e2 @@ seq t0 t1 t0
				| Bneq -> compile_binop env e1 e2 @@ sne t0 t1 t0
        | Blt -> compile_binop env e1 e2 @@ slt t0 t1 t0
				| Blte -> compile_binop env e1 e2 @@ sle t0 t1 t0
				| Bgt -> compile_binop env e1 e2 @@ sgt t0 t1 t0
				| Bgte -> compile_binop env e1 e2 @@ sge t0 t1 t0
        | Band -> compile_binop env e1 e2 @@ and_ t0 t1 t0
				| Bor -> compile_binop env e1 e2 @@ or_ t0 t1 t0
        | Badd -> compile_binop env e1 e2 @@ add t0 t1 oreg t0
        | Bsub -> compile_binop env e1 e2 @@ sub t0 t1 oreg t0
				| Bmul -> compile_binop env e1 e2 @@ mul t0 t1 oreg t0
				| Bdiv -> compile_binop env e1 e2 @@ beqz t0 "cerr_div_by_zero" @@ div t0 t1 oreg t0
				| Bmod -> compile_binop env e1 e2 @@ beqz t0 "cerr_div_by_zero" @@ rem t0 t1 oreg t0
        )
	|	Ecall (lval, args) ->
		(match lval with
		| Lident f -> 
			let comp = String.compare f.node "System$out$print" in
			if comp = 0 then 
				let expr_ = List.hd args in
				let cexpr = compile_expr env expr_ in
				match expr_.info with
				| Tint -> cexpr @@ jal "print_int" @@ print_str "backslashn"
				| Tboolean -> 
					let code1 = print_str "btrue" @@ print_str "backslashn" in
					let code2 = print_str "bfalse" @@ print_str "backslashn" in
					cexpr @@ compile_cond code1 code2
				| Tclass "String" -> assert false
				| _ -> assert false		
			else assert false
		| _ -> assert false)
	| _ -> assert false
and compile_binop env e1 e2 =
					compile_expr env e1 @@ push t0 @@ compile_expr env e2 @@ pop t1

(* [type_opt env oe] génère le code d'une expression contenue dans un option *)
let type_opt env oe =
  match oe with
    None -> assert false
  | Some e -> assert false

let rec compile_instr instr =
	match instr.node with
  | Iexpr e -> compile_expr [] e
  | Idecl (t, x, eopt) -> assert false
  | Iif (e, i1, i2) ->  
					let cexpr = compile_expr [] e in
					let code1 = compile_instr i1 in
					let code2 = compile_instr i2 in
					cexpr @@ compile_cond code1 code2
  | Ifor (oe1, oe2, oe3, i') ->
		(* t0 = oe2 ; t1 = oe3 ; t2 = oe1*)
		(* la boucle ne va réévaluer que oe3*)
		(* donc t0=oe3 puisque la compilation des expressions stockent dans t0 *)
		(match oe1,oe2,oe3 with
		| Some e1, Some e2, Some e3 ->
		let finit = compile_expr [] e1 in
		let move_init = finit @@ move t2 t0 in
		let fstate = compile_expr [] e3 in
		let move_state = fstate @@ move t1 t0 in
		let fcond = compile_expr [] e2 in
		let code1 = compile_instr  i' in
			compile_for move_init fcond move_state code1
		| _ -> assert false)
  | Iblock li ->
		List.fold_left (fun ins lins -> ins @@ compile_instr lins) nop li
	| _ -> comment "c'est le main"

let prog (class_list, main_class, main_body) =
	let body_code = compile_instr main_body in
	{
		text =
			label "main"
			@@ comment "c'est le main"
			@@ body_code
			@@ b "end"
			@@ print_int
			@@ print_string
			@@ cerr_div_by_zero
			@@ end_;
		data = data_label;
	}