open Ast
open Mips

(* let prog_end =       *)
(* 	label "end" @@ nop *)
	

let div_by_zero =
	label "div_by_zero" @@ asciiz "division par zéro"
						
					
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
				| Bdiv -> compile_binop env e1 e2 @@ div t0 t1 oreg t0
				| Bmod -> compile_binop env e1 e2 @@ rem t0 t1 oreg t0
        )  
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
  | Iif (e, i1, i2) -> compile_expr [] e @@
											 beqz t0 "else1" @@
											compile_instr i1 @@
											b "end1" @@
											label "else1" @@
											compile_instr i2 @@
											label "end1"
  | Iblock li ->
		let rec aux l =
			match l with
			| [] -> nop
			| a :: r -> compile_instr a @@ aux r
		in aux li
	| _ -> comment "c'est le main"

let prog (class_list, main_class, main_body) =
	let body_code = compile_instr main_body in
	{
		text =
			label "main"
			@@ comment "c'est le main"
			@@ body_code
			@@ add a0 t0 oi 0
			@@ li v0 1
			@@ syscall;
		data = nop;
	}