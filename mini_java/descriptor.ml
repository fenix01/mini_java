open Ast
open Mips

(* GENERATION DES DESCRIPTEURS DE CLASSES *)

type class_addr = {
	mutable name : string;
	attrs : (string,int) Hashtbl.t ;
	methods : (string,int) Hashtbl.t ;
	mutable attrs_shift : int;	
	mutable methods_shift : int;	
	mutable descriptor : data;
	}

type classes_addr = {
	c : (string,class_addr) Hashtbl.t;
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
		name = "Object";
		attrs = Hashtbl.create 10;
		attrs_shift = 4;
		methods = Hashtbl.create 10;
		methods_shift = 4;
		descriptor = label "_desc$Object" @@ dword [0];
	} in 
	classes_addr.descriptors <- classes_addr.descriptors @@ this_addr.descriptor;
	Hashtbl.add classes_addr.c "Object" this_addr
	
let class_string = 
		let this_addr = {
		name = "String";
		attrs = Hashtbl.create 10;
		attrs_shift = 4;
		methods = Hashtbl.create 10;
		methods_shift = 8;
		descriptor = 	label "_desc$String" @@ address ["_desc$Object"] @@ address ["_method$String$equals$Object"];
	} in
		classes_addr.descriptors <- classes_addr.descriptors @@ this_addr.descriptor;
	Hashtbl.add classes_addr.c "Object" this_addr

let str_type type_ =
	match type_ with
	| Tnull -> "null"
	| Tint -> "int"
	| Tboolean -> "boolean"
	| Tclass cname -> cname
	| Tvoid -> "void"

let class_desc class_name = "_desc$"^class_name

let attr_desc class_name attr_name = class_name^"$"^attr_name

let method_desc class_name method_name method_type params =
	let prolog = method_type ^ class_name ^ "$" ^ method_name in
	let epilog = List.fold_left (
				fun desc param ->
						let type_, _ = param in
						desc ^ "$" ^ str_type type_
			) "" params
	in prolog ^ epilog
	
let get_this_addr this_name =
	try
		Hashtbl.find classes_addr.c this_name
	with Not_found -> failwith "error peu commune"
	
let get_attr_addr this_addr attr_name =
	try
		Hashtbl.find this_addr attr_name
	with Not_found -> failwith "error peu commune"
	
let get_method_addr this_addr method_name =
	try
		Hashtbl.find this_addr method_name
	with Not_found -> failwith "error peu commune"

let build_descriptor this parent defns =
	let this_addr = {
		name = this.node;
		attrs = Hashtbl.create 10;
		attrs_shift = 4;
		methods = Hashtbl.create 10;
		methods_shift = 4;
		descriptor = nop;
	}
	in 
	let rec aux this defns =
	match defns with
	| [] -> ()
	| el :: r ->
			let _ =
				match el with
				| Dconstr (f, params, i ) ->  
					let desc_name = method_desc this.node f.node "_ctor$" params in
					Printf.printf "%s\n" desc_name;
					let desc_ = address [desc_name] in
						Hashtbl.add this_addr.methods desc_name this_addr.methods_shift;
						this_addr.methods_shift <- this_addr.methods_shift + 4;
						this_addr.descriptor <- this_addr.descriptor @@ desc_; 
					
				| Dmeth (ret, f, params, i ) ->
					let desc_name = method_desc this.node f.node "_method$" params in
					Printf.printf "%s\n" desc_name;
					let desc_ = address [desc_name] in
						Hashtbl.add this_addr.methods desc_name this_addr.methods_shift;
						this_addr.methods_shift <- this_addr.methods_shift + 4;
						this_addr.descriptor <- this_addr.descriptor @@ desc_; 
				| Dfield (typ, x) -> 
						Printf.printf "%s\n" (attr_desc this.node x.node);
						Hashtbl.add this_addr.attrs (attr_desc this.node x.node) this_addr.attrs_shift;
						this_addr.attrs_shift <- this_addr.attrs_shift + 4;
			in
			aux this r
		in 
	aux this defns;
	Printf.printf "%d\n" (this_addr.attrs_shift);
	let p_desc = class_desc parent.node in
	let t_desc = class_desc this.node in
	classes_addr.descriptors <- classes_addr.descriptors @@ label t_desc 
	@@ address [p_desc] @@ this_addr.descriptor;
	Hashtbl.add classes_addr.c this.node this_addr
			
let build_descriptors class_list =
	let rec descriptors clist =
		match clist with
		| [] -> ()
		| (this, parent, defns) :: r -> 
			build_descriptor this parent defns ; descriptors r
	in descriptors class_list
	
	

(* ######################################################## *)