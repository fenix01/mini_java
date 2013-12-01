open Ast
open Error

module MethodSignature =
  struct
    (* name * arguments *)
    type t = string * typ list
    let compare = compare
  end

module MethodSet = Set.Make(MethodSignature)



type method_desc = {
  return : typ;
  (* type de retour de la méthode *)
  class_def : string;
  (* classe dans laquelle est définie la méthode *)
  class_override : string;
  (* classe où était définie la première version de la méthode *)
}

module MethodMap = struct
  include Map.Make(MethodSignature)
  let print ppf m =
    let l = bindings m in
    List.iter (fun ((f, s), d) ->
      Format.fprintf ppf "%s (%s, %s)\n" f d.class_override d.class_def
    ) l
end

let mangle =
    let buf = Buffer.create 128 in
    fun prefix cls f tlist ->
      Buffer.reset buf;
      Buffer.add_string buf prefix;
      Buffer.add_char buf '$';
      Buffer.add_string buf cls;
      Buffer.add_char buf '$';
      Buffer.add_string buf f;
      List.iter (fun t ->
        Buffer.add_char buf '$';
        Buffer.add_string buf (typ_to_string t))
        tlist;
      Buffer.contents buf

type class_desc = {
  name : string;
  (* nom de la classe *)
  parent: string;
  (* nom du parent, "" pour le parent de Object *)
  mutable fields : (string * (typ * string)) list;
  (* liste des attributs, leur type et la classe dans laquelle ils
     sont définis *)
  mutable methods : method_desc MethodMap.t;
  (* Table qui donne pour chaque méthode (identifiée par son nom et sa
     signature) les informations de méthode (type method_desc) *)
  mutable ctors : method_desc MethodMap.t;
  (* Pareil pour les constructeurs mais le method_desc contient des
     valeurs bidons (le type de retour est Tvoid, la classe de
     définition "" et la classe de définition initiale "")
     Cela permet de partager le code de select_method et select_const.
  *)
}

let ctor_desc = { return = Tvoid; class_def = ""; class_override = "" }

let empty_desc = { name = "";
                   parent = "";
                   fields = [];
                   methods = MethodMap.empty;
                   ctors = MethodMap.empty;
                 }

let object_desc = { empty_desc with name = "Object"  }


let equals_sig = "equals", [ Tclass "Object" ]
let equal_desc = { return = Tboolean;
                   class_def = "String";
                   class_override = "String";
                 }
let string_desc = { name = "String";
                    parent = "Object";
                    fields = [];
                    methods =
                       MethodMap.add
                         equals_sig equal_desc
                         MethodMap.empty;
                    ctors = MethodMap.empty;
                  }

let mk_dummy v = { node = v; info = Lexing.dummy_pos, Lexing.dummy_pos }


let object_ast = (mk_dummy "Object"), (mk_dummy ""), []


let class_table = Hashtbl.create 17

(* Tri topologique (version fonctionnelle).
   Pré-condition: toutes les classes ainsi que leur parents existent
   dans class_table
*)
let topological_sort class_list =
  let rec loop s l e =
    match s, e with
      [], [] -> List.tl (List.rev l) (* On renverse la liste et on
                                        retire object_ast qu'on a
                                        placé en tête *)
    | [], (cname,_,_) :: _ -> error (Cyclic_inheritance cname.node) cname.info
    | ((cname, _, _) as cls) ::ss, _ ->
        let ll = cls :: l in
        let ee, ss =
          List.fold_left (fun (acee, acss) ((_, par2, _) as cls2) ->
            if par2.node = cname.node then
              (acee, cls2 :: acss)
            else
              (cls2 :: acee, acss)
          ) ([], ss) e
        in
        loop ss ll ee
  in
  loop [ object_ast ] [] class_list

let rec subclass c1 c2 =
  if c1 = "" then false
  else
    let c1_desc = Hashtbl.find class_table c1 in
    c1_desc.parent = c2 || subclass c1_desc.parent c2

let subtype t1 t2 =
  match t1, t2 with
    Tboolean, Tboolean | Tint, Tint
  | Tnull, Tnull
  | Tvoid, Tvoid -> true
  | Tclass c1,  Tclass c2 -> c1 = c2 || subclass c1 c2
  | Tnull, Tclass _ -> true
  | _ -> false

let compatible t1 t2 = subtype t1 t2 || subtype t2 t1

let wf t =
  match t with
    Tclass c -> c = "Object" || c = "String" || Hashtbl.mem class_table c
  | Tint | Tboolean -> true
  | _ -> false

let check_wf t loc =
  if not (wf t) then error (Invalid_type t) loc

(* Variante de List.for_all2 qui renvoie false quand les
   listes sont de tailles différentes au lieu de lever une
   exception
*)
let for_all2 f l1 l2 =
  try
    List.for_all2 f l1 l2
  with
    Invalid_argument _ -> false

let min_meth f a1 a2 =
  let _, sig1, _, _ = a1 in
  let _, sig2, _, _ = a2 in
  if for_all2 subtype sig1 sig2 then a1
  else if for_all2 subtype sig2 sig1 then a2
  else error (Ambiguous_method_call f.node) f.info

(* select_method telle que demandée dans l'énoncé *)
let select_by_sig f targs map =
  let candidates =
    (* on selectionne tous les candidats potentiels:
       toutes les méthodes ayant le même nom que la méthode demandée
       et dont le profil convient (est un supertype) des types des
       arguments *)
    MethodMap.fold (fun (g,gargs) d acand ->
      if g = f.node && for_all2 subtype targs gargs then
        (g, gargs, d.return, d.class_override) :: acand
      else acand
    ) map []
  in
  (* On a obtenu une liste de candidats: *)
  match candidates with
    [] -> error (No_candidate_method f.node) f.info
  | [ m ] -> m
  | c :: r -> List.fold_left (min_meth f) c r

let select_method cls f targs =
  select_by_sig f targs (Hashtbl.find class_table cls).methods

let select_constr cls targs =
  select_by_sig cls targs (Hashtbl.find class_table cls.node).ctors

let select_field c x =
  let c_desc = Hashtbl.find class_table c in
    try
      List.assoc x.node c_desc.fields
    with
      Not_found ->
        error (Invalid_field_access x.node) x.info

module StringSet = Set.Make(String)

let check_params params =
  let s = ref StringSet.empty in
  List.map (fun (t, x) ->
    check_wf t x.info;
    if StringSet.mem x.node !s then
      error (Already_defined x.node) x.info
    else begin
      s := StringSet.add x.node !s;
      t
    end) params

let init_class_table class_list =
  Hashtbl.clear class_table;
  (* Les deux classes prédéfinies *)
  Hashtbl.replace class_table "Object" object_desc;
  Hashtbl.replace class_table "String" string_desc;
  (* Pour chaque classe de l'AST, on ajoute une descripteur (vide)
     dans la table *)
  List.iter (fun (cl, par, _) ->
    if Hashtbl.mem class_table cl.node then
      error (Class_redefinition cl.node) (cl.info);
    if par.node = "String" then
      error (Cannot_extend_string cl.node) (par.info);
    Hashtbl.add class_table cl.node
      { empty_desc with
        name = cl.node;
        parent = par.node
      }
  ) class_list;
  (* On vérifie que le parent de chaque classe existe *)
  List.iter (fun (_, parent, _) ->
    if not (Hashtbl.mem class_table parent.node) then
      error (Undefined_class parent.node) parent.info
  ) class_list;
  (* On effectue un tri topologique des classes *)
  let sorted_class_list = topological_sort class_list in
    (* On remplit le déscripteur de classe pour chaque classe du fichier *)
  List.iter (fun (this, parent, defns) ->
    let parent_desc = Hashtbl.find class_table parent.node in
    let atts, methods, ctors =
      (* On vérifie chaque définition et on l'ajoute à l'ensemble
         des attributs/methodes/constructeurs
      *)
      List.fold_left (fun (a_atts, a_meths, a_ctors) def ->
        match def with
          Dfield (t, x) ->
            check_wf t x.info;
            if List.mem_assoc x.node a_atts then
              error (Redefined_attribute (this.node,x.node)) x.info
            else (x.node,(t,this.node)) :: a_atts, a_meths, a_ctors
        | Dconstr (f, params, _ ) ->
            let tparams = check_params params in
            if f.node <> this.node then
              error (Invalid_constructor(this.node, f.node)) f.info
            else if MethodMap.mem (f.node, tparams) a_ctors then
              error (Redefined_constructor this.node) f.info
            else a_atts, a_meths, (MethodMap.add (f.node,tparams) ctor_desc
                                     a_ctors)
        | Dmeth (ret, f, params, _) ->
            let tparams = check_params params in
            let msig = f.node, tparams in
            let new_meth_info =
            try
              let meth_info = MethodMap.find msig a_meths in
              (* s'il existe une méthode de même signature dans la
                 classe que l'on est en train de définir, c'est une
                 erreur *)
              if meth_info.class_def = this.node then
                error (Redefined_method (this.node, f.node)) f.info
              else
                if not (compatible meth_info.return ret) then
                  error
                    (Invalid_override
                       (f.node, this.node,
                        meth_info.class_override, ret, meth_info.return)
                    ) f.info
                else
                  {meth_info with
                    class_def = this.node;
                  }
            with
              Not_found ->
                { return = ret;
                  class_def = this.node;
                  class_override = this.node;
                }
            in
            a_atts, MethodMap.add msig new_meth_info a_meths, a_ctors
      ) ([], parent_desc.methods, MethodMap.empty) defns
    in
    let this_desc = Hashtbl.find class_table this.node in
    this_desc.fields <- List.rev_append atts parent_desc.fields;
    this_desc.methods <- methods;
    this_desc.ctors <- ctors
  ) sorted_class_list;
  (* enfin, on renvoie la liste triée *)
  sorted_class_list
