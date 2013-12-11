open Mips

(* LISTE DES GENERATEURS D'ETIQUETTE *)
let next = let r = ref 0 in fun () -> r:= !r +1; !r
let cond i = Printf.sprintf "cond%i" i
let endcond i = Printf.sprintf "endcond%i" i
let gen_str = let r = ref 0 in fun () -> r:= !r +1; !r
let next_str () = let i = gen_str() in Printf.sprintf "str%i" i
(* ######################################################## *)

let pushad = push t0 @@ push t1 @@ push t2 @@ push t3 @@ push v0
let popad = pop v0 @@ pop t3 @@ pop t2 @@ pop t1 @@ pop t0
let switch r1 r2 = push r1 @@ move r1 r2 @@ pop r2

let caller_method2 register args size =
	let push_treg = comment "caller init" @@ pushad in
	let pop_args = add sp sp oi size in
	let pop_treg = comment "caller final" @@ popad
	@@ comment "caller final2"
	in
	push_treg @@ args @@ jalr register @@ move v1 v0 @@ pop_args @@ pop_treg

let caller_method label_name =
	let push_treg = comment "caller init" @@ pushad in
	let pop_treg = comment "caller final" @@ popad in
	push_treg @@ jal label_name @@ move v1 v0 @@ pop_treg
	
let callee_method loc_size code =
	let init = comment "calle init" @@ push fp @@ push ra in
	let init_fp = sub fp sp oi loc_size @@ move sp fp in
	let final = comment "calle final" @@ add sp fp oi loc_size @@ pop ra @@ pop fp @@ jr ra
	in init @@ init_fp @@ comment "callee code" @@ code @@ final
	
let alloc_mem class_desc size =
	li a0 size @@ li v0 9 @@ syscall @@ move t0 v0 @@ move t1 v0 @@
	la t1 alab class_desc @@ sw t1 areg(0,t0)

(* code qui effectue un if then else et qui branche sur code 1 ou code 2 *)
let compile_cond code1 code2 =
	let i = next() in
	beqz t0 (cond i) @@ code1 @@ b (endcond i) @@ label (cond i) @@ code2 @@ label (endcond i)

(* code qui effectue un for et qui branche sur code 1 *)
let compile_for e1' e2' e3' code1 =
	let i = next() in
	e1' @@ label(cond i) @@ e2' @@ beqz t0 (endcond i) @@ code1 @@ e3' @@ b (cond i) @@ label (endcond i)
	
let alloc_str reg =
		push v0 @@ 
		move a0 reg @@
		li v0 9 @@
		syscall @@
		move v1 v0 @@
		pop v0	
	
let copy linit winit length reg =
	let e1' = 
		move s0 linit @@ 
		move s1 length
	in
	let e2' = slt t0 s0 s1 in
	let e3' = add s0 s0 oi 1 in
	let code = 
		push reg @@ 
		push v1 @@
		add reg reg oreg s0 @@ 
		add v1 v1 oreg s0 @@ 
		add v1 v1 oreg winit @@
		lbu a0 areg(0,reg) @@ 
		sb a0 areg(0,v1) @@
		pop v1 @@
		pop reg in
	push t0 @@ compile_for e1' e2' e3' code @@ pop t0
	
(* la fonction prend un paramètre en entrée *)
let is_equal =
	let equal_ =
		la t0 areg(8,fp) @@
		la t1 areg(12,fp) @@
		lw t0 areg(4,t0) @@
		lw t1 areg(4,t1) @@
		sne t0 t0 t1 @@
		compile_cond (li t0 1 @@ move v0 t0) (li t0 0 @@ move v0 t0)
	in label "_meth$String$equals$Object" @@ callee_method 0 equal_

let concatenate_string =
	let concatenate =
	(* t2 = longueur de la chaîne 2*)
	(* t3 = longueur de la chaîne 1*)
	lw t2 areg(8,t0) @@ 
	lw t3 areg(8,t1) @@ 
	(* t0 = pointeur vers String 2*)
	(* t1 = pointeur vers String 1*)
	lw t0 areg(4,t0) @@
	lw t1 areg(4,t1) @@
	(* t2 = longueur str1 + longueur str2*)
	add t2 t3 oreg t2 @@
	(* t0 = addresse du nouveau String *)
	push t0 @@ 
	push t1 @@
	alloc_mem "_desc$String" 12 @@
	pop t1 @@
	pop t0 @@
	(* stocke la longueur de la nouvelle chaîne sans \0 *)
	sw t2 areg(8,v0) @@
	(* ajoute 1 à longueur de la nouvelle chaîne pour le caractère de fin \0 *)
	add t2 t2 oi 1 @@
	(* v1 = mémoire allouée pour la nouvelle chaîne *)
	alloc_str t2 @@
	(* stocke l'addresse de la nouvelle chaîne dans le nouvelle object *)
	sw v1 areg(4,v0) @@
	(* écrit byte à byte dans la nouvelle chaîne *)
	copy zero zero t3 t1 @@
	add t3 t3 oi 1 @@
	sub t2 t2 oreg t3 @@
	move t3 t2 @@
	move t1 t0 @@
	move a1 s1 @@
	comment "copy 2" @@
	copy zero a1 t3 t1
	in 	label "concatenate_str" @@ callee_method 0 concatenate
	
(* code généré par notre compilateur pour avoir la structure pour convertir facilement*)
(* un entier en string *)
let struct_conv compute_ascii compute_size =
	sw t0 areg(0,fp) @@
	sw t0 areg(4,fp) @@
	sw t0 areg(8,fp) @@
	label "cond4" @@
	lw t0 areg(0,fp) @@
	sub sp sp oi 4 @@
	sw t0  areg(0,sp) @@
	li t0 0 @@
	lw t1 areg(0,sp) @@
	add sp sp oi 4 @@
	sgt t0 t1 t0 @@
	beqz t0 "endcond4" @@
	lw t0 areg(0,fp) @@
	sub sp sp oi 4 @@
	sw t0  areg(0,sp) @@
	li t0 10 @@
	lw t1  areg(0,sp) @@
	add sp sp oi 4 @@
	beqz t0 "cerr_div_by_zero" @@
	rem t0 t1 oreg t0 @@
	sub sp sp oi 4 @@
	sw t0  areg(0,sp) @@
	li t0 48 @@
	lw t1  areg(0,sp) @@
	add sp sp oi 4 @@
	add t0 t1 oreg t0 @@
	sw t0 areg(4,fp) @@
	lw t0 areg(4,fp) @@
	compute_ascii @@
	lw t0 areg(0,fp) @@
	sub sp sp oi 4 @@
	sw t0 areg(0,sp) @@
	li t0 10 @@
	lw t1  areg(0,sp) @@
	add sp sp oi 4 @@
	beqz t0 "cond3" @@
	div t0 t1 oreg t0 @@
	b "endcond3" @@
	label "cond3" @@
	b "cerr_div_by_zero" @@
	label "endcond3" @@
	sw t0 areg(0,fp) @@
	lw t0 areg(8,fp) @@
	add t1 t0 oi 1 @@
	sub sp sp oi 4 @@
	sw t0 areg(0,sp) @@
	move t0 t1 @@
	lw t1 areg(0,sp) @@
	add sp sp oi 4 @@
	sw t0 areg(8,fp) @@
	sub sp sp oi 4 @@
	sw t1 areg(0,sp) @@
	move t1 t0 @@
	lw t0 areg(0,sp) @@
	add sp sp oi 4 @@
	b "cond4" @@
	label "endcond4" @@
	compute_size
	
(* utilise la structure de convertion pour connaître la longueur de l'entier *)
let meth_int_length =
	let comp_ascii = nop in
	let comp_length = lw t0 areg(8,fp) @@ move v0 t0 in
	label "int_length" @@ callee_method 12 (struct_conv comp_ascii comp_length)
	
(* utilise la structure de convertion pour stocker les caractères *)
let meth_int_str =
	let comp_ascii = 
		lw t1 areg(8,fp) @@
		add a0 a0 oreg t1 @@
		sb t0 areg(0,a0) in
	let comp_length = nop in
	label "int_str" @@ 
	move a0 v0 @@
	callee_method 12 (struct_conv comp_ascii comp_length)
	
let convert_int =
	caller_method "int_length" @@
	alloc_mem "_descString" 12 @@
	sw v1 areg(8,v0) @@
	alloc_str v1 @@
	sw v1 areg(4,v0) @@
	caller_method "int_str" @@
	move t0 v1
	
	
	