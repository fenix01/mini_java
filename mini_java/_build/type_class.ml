open Ast

exception Class_error of string * position

let error s p = raise (Class_error (s, p))

type class_info =
	{ name : string; parent : string;
		(* nom de variable avec couple (type, nom de la classe) *)
		mutable attributes : (string * (typ * string)) list;
		mutable cotrs : ((typ * string) list) list;
		mutable methods : (string * (typ * ((typ * string) list) * string)) list
	}

(* représente la class object à l'aide du type class_info *)
let object_info =
	{
		name = "Object";
		parent = "";
		attributes = [];
		cotrs = [];
		methods =
			[ "equals", (Tboolean, [ ((Tclass "String"), "a") ], "String") ];
	}

(* représente la class string à l'aide du type class_info *)
let string_info =
	{
		name = "String";
		parent = "Object";
		attributes = [];
		cotrs = [];
		methods =
			[ "equals", (Tboolean, [ ((Tclass "String"), "a") ], "String") ];
	}

(* représente un hashtable *)
let class_table =
	let h = Hashtbl.create 17
	in
	(Hashtbl.add h "Object" object_info;
		Hashtbl.add h "String" string_info;
		h)
			
(* représente un hashtable temporaire *)
let tmp_class_table = 
	let h = Hashtbl.create 17
	in
	(Hashtbl.add h "Object" object_info;
		Hashtbl.add h "String" string_info;
		h)

(* récupère une classe_info en fonction de son nom *)
let get_class class_name =
	try
		Hashtbl.find class_table class_name
	with Not_found -> failwith "erreur critique : la classe n'existe pas."

(* récupère une méthode en fonction de son nom *)
let get_method class_info method_name =
	try
		List.assoc method_name class_info.methods
	with Not_found -> failwith "erreur critique : la méthode n'existe pas."

let dummy_pos2 = (Lexing.dummy_pos, Lexing.dummy_pos)

let object_ast =
	({ node = "Object"; info = dummy_pos2; },
		{ node = ""; info = dummy_pos2; }, [])

(* select_field permet de connaître le type d'un attribut il suffit de     *)
(* fournir le nom de la classe et le nom de l'attribut                     *)
let select_field class_name attribute_name =
	let class_ = get_class class_name in
	try
		List.assoc attribute_name class_.attributes
	with
	| Not_found -> failwith ("l'attribut " ^ attribute_name ^ "n'existe pas")

(* effectue un trie topologique sur le graphe d'héritage *)
let topological_sort (clist : (position klass) list) =
	let l = ref [] in
	let s = ref [ object_ast ] in
	let graph = ref clist
	in
	(while !s <> [] do
			(let n = List.hd !s in
				let (cname, _, _) = n
				in
				(s := List.tl !s;
					l := n :: !l;
					graph :=
					List.fold_left
						(fun agraph m ->
									let (mname, _, _) = m in
									let minfo =
										try Hashtbl.find class_table mname.node
										with
										| Not_found ->
												error
													("La classe " ^
														(mname.node ^ " parent n'existe pas"))
													mname.info
									in
									if minfo.parent = cname.node
									then (s := m :: !s; agraph)
									else m :: agraph)
						[] !graph))
		done;
		if !graph <> []
		then
			(let (class_, class_parent, _) = List.hd !graph
				in
				error ("définition d'une classe cyclique : " ^ class_parent.node)
					class_parent.info)
		else ();
		List.tl (List.rev !l))

(* fonction qui copie les déclarations de la classe parent dans la classe  *)
(* enfant                                                                  *)
let copy_parent_decls class_ class_parent =
	if class_parent.name = ""
	then class_
	else
		(class_.attributes <- class_parent.attributes;
			class_.cotrs <- class_parent.cotrs;
			class_.methods <- class_parent.methods;
			class_)

(* fonction vérifiant l'existence d'une classe *)
let class_exists class_name =
	Hashtbl.mem class_table class_name

(* fonction vérifiant l'existence d'un attribut *)
let exist_attr class_info attr_name =
	List.mem_assoc attr_name class_info.attributes

(* fonction vérifiant l'existence d'une méthode *)
let method_exist class_info method_name =
	List.mem_assoc method_name class_info.methods

let is_function type_ =
	match type_ with
	| Tvoid -> true
	| _ -> false

(* let rec check_function instr =            *)
(* 		let instr_node = instr.node in        *)
(* 		let block = instr_node.node in        *)
(* 		match instr with                      *)
(* 		| _ -> true                           *)
(* 		| _ :: r -> false && check_function r *)

(* génère la méthode à partir des définitions de l'ast *)
let create_method method_ class_name =
	let type_, name_, params = method_ in
	let params_ = List.fold_left ( fun x (type_, name_) -> (type_, name_.node):: x) [] params in
	let rev = List.rev params_ in
	name_.node, (type_, rev, class_name)

(* vérifie que les paramètres d'une méthode ou d'un constructeur sont uniques *)
let check_unique_vparam params =
	let _ =
	List.fold_left (
		fun tmp_names (_,name_) ->
			if List.mem name_.node tmp_names then
				error ("Un paramètre " ^ name_.node ^ " existe déjà.") name_.info
			else name_.node :: tmp_names
		) [] params
	in ()
	
(* compare 2 types *)
let compare_types type_ type2_ =
	match type_,type2_ with
		| Tclass cname1,Tclass cname2 -> (cname1 = cname2)
		| Tclass class_name1,_ -> false
		| _,Tclass class_name2 -> false
		| _,_ -> (type_ = type2_)
	
(* vérifie que les types sont identiques pour 2 listes de paramètres *)
let rec check_types plist1 plist2 =
	match plist1, plist2 with
	| [],[] -> true
	| [], _ -> false
	| _,[] -> false
	| (type_, _):: r, (type2_, _):: s ->
										(compare_types type_ type2_) && check_types r s	
	
let check_method_signature method_ class_info =
	let type_,name_,params_ = method_ in
	let length1 = List.length params_ in
	List.iter (
		fun (mname_,(mtype_,mparams_,mclass_name)) ->
			let length2 = List.length mparams_ in
			 if name_.node = mname_ then                               
				if length1 = length2 && check_types params_ mparams_ then
					error ("méthode dupliquer " ^ mname_) name_.info
	) class_info.methods

(* vérifie que l'utilisation d'un type class existe pour un paramètre donné*)
let check_class_var_exists type_ name_ =
	match type_ with
	| Tclass class_name -> if not (class_exists class_name) then
				error ("la classe " ^ class_name ^ " n'existe pas.") name_.info;
	| _ -> ()

(* vérifie que l'utilisation d'un type class existe pour une liste de paramètre *)
let check_class_vlist_exists params =
	List.iter ( fun (type_, name_) ->
					check_class_var_exists type_ name_
		) params

let print type_ =
	match type_ with
		| Tclass cname -> Printf.printf "%s\r\n" cname;
		| Tint -> Printf.printf "int\r\n";
		| Tvoid -> Printf.printf "void\r\n";
		| _ -> ()


(* vérifie que la redéfinition est correcte si une méthode se trouve dans  *)
(* la classe parent                                                        *)
let check_is_parent_method class_info class_parent method_ =
	let type_, name_, params = method_ in
	if method_exist class_parent name_.node then
		let (mtype_, mparams, _) = get_method class_parent name_.node in
		let length1 = List.length params in
		let length2 = List.length mparams in
		if (length1 = length2) && check_types mparams params
		&& not (compare_types mtype_ type_) then
			error ("type de retour invalide lors de redéfinition de la méthode " ^ name_.node) name_.info

(* méthode qui vérifie si un constructeur avec la même définition existe   *)
(* déjà dans la lise                                                       *)
let exist_constr class_info params =
	let length1 = List.length params in
	let rec check_constrs clist =
		match clist with
		| [] -> false
		| a :: r ->
				let length2 = List.length a in
				if length1 <> length2 then
					false || check_constrs r
				else
					check_types a params || check_constrs r
	in check_constrs class_info.cotrs

(* génère le constructeur à partir de la liste des paramètres de l'ast *)
let create_cotrs cparams_ =
	List.fold_left ( fun x (type_, name_) -> (type_, name_.node):: x) [] cparams_

(* parcours la liste des déclarations de la classe *)
let rec parse_declarations cdecl class_info class_parent =
	match cdecl with
	| [] -> class_info
	| Dfield (type_, name_) :: r ->
	(* vérifie que l'attribut n'existe pas dans la classe parent *)
			if exist_attr class_parent name_.node then
				error ("l'attribut " ^ name_.node ^ " a été redéfini") name_.info;
			(* vérifie que le type de l'attribut si c'est une classe existe *)
			check_class_var_exists type_ name_;
			
			class_info.attributes <- (name_.node, (type_, class_info.name)):: class_info.attributes;
			parse_declarations r class_info class_parent
	| Dconstr (name_, cparams_, _) :: r ->
	(* vérifie dans un premier temps que le constructeur a le même nom que   *)
	(* la classe                                                             *)
			if name_.node <> class_info.name then
				error ("le constructeur " ^ name_.node ^ " doit avoir le même nom que la classe.") name_.info;
			(* vérifie que les constructeurs sont deux à deux distincts *)
			if exist_constr class_info cparams_ then
				error ("le constructeur " ^ name_.node ^ " a été redéfini.") name_.info;
			(* vérifie les types des paramètres *)
			check_class_vlist_exists cparams_;
			check_unique_vparam cparams_;
			let cotrs = create_cotrs cparams_ in
			class_info.cotrs <- cotrs:: class_info.cotrs;
			
			parse_declarations r class_info class_parent
	| Dmeth (type_, name_, params, instrs) :: r ->
			check_is_parent_method class_info class_parent (type_, name_, params);
			check_class_vlist_exists params;
			check_unique_vparam params;
			(* if is_function type_ then *)
			(* 	check_function instrs;  *)
			check_method_signature (type_, name_, params) class_info;
			let cmethod = create_method (type_, name_, params) class_info.name in
			class_info.methods <- cmethod:: class_info.methods;
			parse_declarations r class_info class_parent

(* fonction qui parse une première fois le nom des classes *)
let rec parse_classes clist =
	match clist with
	| [] -> ()
	| (name_, parent_, decl_class) :: s -> (* impossible d'hériter de string *)
			(if parent_.node = "String"
				then error "Il n'est pas possible d'hériter de String." parent_.info
				else ();
				if Hashtbl.mem class_table name_.node
				then error ("Redéfinition de la classe " ^ name_.node) name_.info
				else
					Hashtbl.add class_table name_.node
						{
							name = name_.node;
							parent = parent_.node;
							attributes = [];
							cotrs = [];
							methods = [];
						};
				parse_classes s)

(* fonction qui parse la liste des classes après le passage du trie        *)
(* topologique                                                             *)
let rec parse_classes2 clist =
	match clist with
	| [] -> ()
	| (name_, parent_, decl_class) :: s -> (* la classe parent n'existe pas *)
			if not (Hashtbl.mem class_table parent_.node)
			then
				error ("La classe " ^ (parent_.node ^ " parent n'existe pas"))
					parent_.info;
			
			let class_info =
				{
					name = name_.node;
					parent = parent_.node;
					attributes = [];
					cotrs = [];
					methods = [];
				} in
			let class_parent =
				(try Hashtbl.find class_table parent_.node
				with | Not_found -> failwith "erreur fatale") in
			let class_info_parent = copy_parent_decls class_info class_parent in
			let check_class_info =
				parse_declarations decl_class class_info_parent class_parent
			in
				Hashtbl.add class_table name_.node check_class_info;
				parse_classes2 s

let init_table prog =
	let (clist, _, _) = prog
	in
	parse_classes clist;
		let clist2 = topological_sort clist
		in
			Hashtbl.reset class_table;
			Hashtbl.add class_table "Object" object_info;
			Hashtbl.add class_table "String" string_info;
			parse_classes clist2;
			parse_classes2 clist2
