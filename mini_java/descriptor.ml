open Ast
open Mips
open Type_class

(* GENERATION DES DESCRIPTEURS DE CLASSES *)

type class_addr = {
	mutable name_ : string;
	attrs_desc : (string, int) Hashtbl.t ;
	methods_desc : (string, int) Hashtbl.t ;
	mutable attrs_shift : int;
	mutable methods_shift : int;
	mutable descriptor : data;
}

type classes_addr = {
	c : (string, class_addr) Hashtbl.t;
	mutable descriptors : data
}

let classes_addr =
	let classes_ = {
		c = Hashtbl.create 10;
		descriptors = nop;
	}
	in classes_

let class_object =
	let this_addr = {
		name_ = "Object";
		attrs_desc = Hashtbl.create 10;
		attrs_shift = 4;
		methods_desc = Hashtbl.create 10;
		methods_shift = 4;
		descriptor = label "_desc$Object" @@ dword [0];
	} in
	classes_addr.descriptors <- classes_addr.descriptors @@ this_addr.descriptor;
	Hashtbl.add classes_addr.c "Object" this_addr

(* le pointeur de chaîne est au décallage 4, la longueur de la chaîne est au décalage 8*)
let class_string =
	let this_addr = {
		name_ = "String";
		attrs_desc = Hashtbl.create 10;
		attrs_shift = 12;
		methods_desc = Hashtbl.create 10;
		methods_shift = 12;
		descriptor = label "_desc$String" @@ address ["_desc$Object"] @@ address ["_method$String$equals$Object"];
	} in
	classes_addr.descriptors <- classes_addr.descriptors @@ this_addr.descriptor;
	Hashtbl.add classes_addr.c "String" this_addr

let str_type type_ =
	match type_ with
	| Tnull -> "null"
	| Tint -> "int"
	| Tboolean -> "boolean"
	| Tclass cname -> cname
	| Tvoid -> "void"

let class_desc class_name = "_desc$"^class_name

let attr_desc class_name attr_name = class_name^"$"^attr_name

let method_desc class_name method_name params =
	let prolog = class_name ^ "$" ^ method_name in
	let epilog = List.fold_left (
				fun desc param ->
						desc ^ "$" ^ str_type param
			) "" params
	in prolog ^ epilog

let get_this_addr this_name =
	try
		Hashtbl.find classes_addr.c this_name
	with Not_found -> failwith "error peu commune 1"

let get_attr_addr attrs_addr attr_name =
	try
		Hashtbl.find attrs_addr attr_name
	with Not_found -> failwith "error peu commune 2"

let get_method_addr meth_addr method_name =
	try
		Hashtbl.find meth_addr method_name
	with Not_found -> failwith "error peu commune 3"

let build_ctors_desc this_addr ctors =
	let binds = MethodMap.bindings ctors in
	List.iter (
			fun ((cotr_name, types_), m_desc) ->
					let desc_name = "_ctor$"^(method_desc cotr_name cotr_name types_) in
					let desc_ = address [desc_name] in
					Hashtbl.add this_addr.methods_desc desc_name this_addr.methods_shift;
					this_addr.methods_shift <- this_addr.methods_shift + 4;
					this_addr.descriptor <- this_addr.descriptor @@ desc_;
		) binds

let build_method_desc this_addr methods =
	List.iter (
			fun ((meth_name, types_), m_desc) ->
					let desc_name = "_meth$"^(method_desc m_desc.class_def meth_name types_) in
					let desc_ = address [desc_name] in
					Hashtbl.add this_addr.methods_desc desc_name this_addr.methods_shift;
					Printf.printf "%s %d\n" desc_name this_addr.methods_shift;
					this_addr.methods_shift <- this_addr.methods_shift + 4;
					this_addr.descriptor <- this_addr.descriptor @@ desc_;
		) methods

let compare_sig l1 l2 =
	let rec aux l1' l2' =
		match l1', l2' with
		| [],[] -> true
		| _, [] -> false
		| [], _ -> false
		| a:: r, b:: s -> let str1 = str_type a in
				let str2 = str_type b in
				let comp = String.compare str1 str2 in
				if comp <> 0 then false else
					true && aux r s
	in aux l1 l2

let compare meth1 meth2 =
	let (meth_name1, types_1), m_desc1 = meth1
	and (meth_name2, types_2), m_desc2 = meth2 in
	if m_desc1.class_override = m_desc2.class_override then
		if meth_name1 = meth_name2 then
			0
		else if compare_sig types_1 types_2 then 0 else 1
	else
	if subclass m_desc1.class_override m_desc2.class_override then
		1
	else
		0

let sort_methods methods_ =
	let binds = MethodMap.bindings methods_ in
	List.sort compare binds

let build_attr_desc this_addr fields =
	List.iter (
			fun (attr_name, (_, class_loc)) ->
					Hashtbl.add this_addr.attrs_desc (attr_desc class_loc attr_name) this_addr.attrs_shift;
					this_addr.attrs_shift <- this_addr.attrs_shift + 4;
		) fields

let build_descriptors () =
	try
		Hashtbl.iter (
				fun cname idecl ->
					if cname <> "Object" && cname <> "String" then
					(	let this_addr = {
							name_ = cname;
							attrs_desc = Hashtbl.create 10;
							attrs_shift = 4;
							methods_desc = Hashtbl.create 10;
							methods_shift = 4;
							descriptor = nop;
						}
						in
						let sorted = sort_methods idecl.methods in
						build_ctors_desc this_addr idecl.ctors;
						build_method_desc this_addr sorted;
						build_attr_desc this_addr idecl.fields;
						let p_desc = class_desc idecl.parent in
						let t_desc = class_desc idecl.name in
						classes_addr.descriptors <- classes_addr.descriptors @@ label t_desc 
						@@ address [p_desc] @@ this_addr.descriptor;                                                     
						Hashtbl.add classes_addr.c cname this_addr)
			) class_table
	with
	| Not_found -> Printf.printf "not found"

(* ######################################################## *)
