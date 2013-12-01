open Ast
open Type_class
open Error

let mk_t e t = { e with info = t }

let mk e t = { node = e; info = t }

let type_error l t1 t2 = error (Type_error (t1,t2)) l

let lval_info l =
  match l with
    Lident id | Laccess( _, id) -> id.info

let update_ident l new_id =
  match l with
    Lident id -> Lident {id with node = new_id }
  | Laccess (e, id) -> Laccess (e, {id with node = new_id })

let is_system_out_print l =
  match l with
    Laccess (e, id) when id.node = "print" ->
      begin match e.node with
        Elval (Laccess (e, id)) when id.node = "out" ->
          begin match e.node with
            Elval (Lident id) -> id.node = "System"
          | _ -> false
          end
      | _ -> false
      end
  | _ -> false

module Env = Map.Make(String)

let rec type_expr env e =
  match e.node with
  | Econst (c) -> let t =
                    match c with
                      Cint _   ->  Tint
                    | Cstring _ -> Tclass "String"
                    | Cbool _ -> Tboolean
                    | Cnull -> Tnull
                  in
                  mk (Econst c) t
  | Elval l -> let tl = type_lval env l in
               mk (Elval tl) (lval_info tl)

  | Eassign (l, e) ->
      let te = type_expr env e in
      let tl = type_lval env l in
      let t = lval_info tl in
      if subtype te.info t then mk (Eassign (tl, te)) t
      else type_error e.info t  te.info

  | Ecall (lval, args) ->
      let targs = List.map (type_expr env) args in

      if is_system_out_print lval then
        match targs with
          [ te ] when
              List.exists (compatible te.info)
                [ Tint; Tboolean;Tclass "String"] ->
            (* On crée un Lident fictif. C'est plus pratique pour
               faire la génération de code *)
                  let new_id = "System$out$print" in
                  mk (Ecall (Lident (mk new_id te.info), targs)) Tvoid
        | _ -> error Invalid_print e.info
      else
        let  cls, tl, f =
          match lval with
          | Lident f -> let c =
                          try
                            Env.find "this" env
                          with Not_found -> error This_in_static f.info
                        in c, Laccess (mk (Elval(Lident (mk "this" c)) ) c, (mk f.node c)), f
          | Laccess (e, f) ->
              let te = type_expr env e in
              te.info, Laccess (te, mk f.node te.info) , f
        in
        let _ , tparams, rt, class_override =
          match cls with
            Tclass cname ->
              select_method cname f (List.map (fun x -> x.info) targs)
          | _ -> error (Call_on_non_class_type(f.node,cls)) f.info
        in
        let new_id = mangle "_meth" class_override f.node tparams in
        mk  (Ecall (update_ident tl new_id, targs)) rt

  | Enew (cls, args) ->
      check_wf (Tclass cls.node) e.info;
      let targs = List.map (type_expr env) args in
      let _, sigc, _, _ =
        select_constr cls (List.map (fun x -> x.info) targs)
      in
      let rt = Tclass cls.node in
      let new_id = mangle "_ctor" cls.node cls.node sigc in
      mk (Enew (mk new_id rt, targs)) rt
  | Eunop (Unot, e) ->
      let te = type_expr env e in
      if te.info =  Tboolean then
        mk (Eunop (Unot, te)) Tboolean
      else type_error e.info Tboolean  te.info

  | Eunop (Uneg, e) ->
      let te = type_expr env e in
      if te.info =  Tint then
        mk (Eunop (Uneg, te)) Tint
      else type_error e.info Tint  te.info

  | Eunop (u, e' ) -> begin
    match e'.node with
      Elval l ->
        let tl = type_lval env l in
        let t = lval_info tl in
        if t = Tint then
          mk (Eunop(u, mk (Elval tl) Tint)) Tint
        else type_error e'.info Tint  t
    | _ -> error Not_lvalue e'.info
  end
  | Ebinop (e1, o, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      begin
        match o with
        | Beq | Bneq ->
            if compatible te1.info te2.info then
              mk (Ebinop(te1, o, te2)) Tboolean
            else
              type_error e2.info te1.info te2.info
        | Blt | Blte | Bgt | Bgte ->
            if te1.info <> Tint then
              type_error e1.info Tint te1.info
            else if te2.info <> Tint then
              type_error e2.info Tint te2.info
            else
              mk (Ebinop (te1, o, te2)) Tboolean

        | Bsub | Bmul | Bdiv | Bmod ->
            if te1.info <> Tint then
              type_error e1.info Tint te1.info
            else if te2.info <> Tint then
              type_error e2.info Tint te2.info
            else
              mk (Ebinop (te1, o, te2)) Tint

        | Band | Bor ->
            if te1.info <> Tboolean then
              type_error e1.info Tboolean te1.info
            else if te2.info <> Tboolean then
              type_error e2.info Tboolean te2.info
            else
              mk (Ebinop (te1, o, te2)) Tboolean
        | Badd ->
          match te1.info, te2.info with
            Tint, Tint -> mk (Ebinop (te1, o, te2)) Tint
          | ((Tint|Tclass "String"), (Tint|Tclass "String")) ->
              mk (Ebinop (te1, o, te2)) (Tclass "String")
          | a,b -> error (Invalid_addition (a,b)) e.info

      end
  | Ecast (t, e') ->
      check_wf t e.info;
      let te = type_expr env e' in
      if subtype te.info t then te (* upcast, on peut le supprimer statiquement *)
      else if subtype t te.info then (* downcast, on testera à runtime *)
        mk (Ecast (t, te)) t
      else
        error (Invalid_cast (t, te.info)) e.info

  | Einstanceof (e', typ) ->
      check_wf typ e.info;
      let te = type_expr env e' in
      match te.info with
      | Tnull | Tclass _  ->
          if subtype te.info typ then
            (* Un super-type est une instance *)
            mk (Econst (Cbool true)) Tboolean
          else (* Un sous-type est peut-être une instance... on vérifiera à runtime *)
            if subtype typ te.info then
              mk (Einstanceof (te, typ)) Tboolean
            else (* on sait statiquement que c'est faux *)
              mk (Econst (Cbool false)) Tboolean
      | _ ->
          error (Invalid_instanceof (typ, te.info)) e.info


and type_lval env l =
  match l with
    Lident x -> begin
      try
        let t = Env.find x.node env in
        Lident (mk x.node t)
      with Not_found ->
        try
          let this_class =
            match Env.find "this" env with
              Tclass c -> c
            | _ -> raise Not_found
          in
          let tc = Tclass this_class in
          let t, cdef = select_field this_class x in
          Laccess (mk (Elval (Lident (mk "this" tc))) tc,
                   mk (cdef ^ "$" ^ x.node) t)
        with
          Not_found ->  error (Unbound_identifier x.node) x.info
    end
  | Laccess (e, x) ->
      let te = type_expr env e in
      match te.info with
      | Tclass c -> let t, cdef = select_field c x in
                    Laccess (te, mk (cdef ^ "$" ^ x.node) t)
      | _ -> error (Invalid_field_access x.node) x.info

(* [type_opt env t e] type une expression contenue dans un option et
   renvoie son type, ou [t] si l'option vaut None *)
let type_opt env t oe =
  match oe with
    None -> t, None
  | Some e -> let te = type_expr env e in
              te.info, Some te


(* [type_instr ret env i] renvoie un triplet [ti, env, b] où ti est
   l'instruction typée, env l'environnement de typage et b un booléen
   qui vaut vrai si tous les chemin d'exécution contiennent un return
*)
let rec type_instr ret env i =
  match i.node with
  | Iexpr e -> let te = type_expr env e in
               mk (Iexpr te) te.info, env, false
  | Idecl (t, x, eopt) ->
      check_wf t i.info;
      if Env.mem x.node env then
        error (Already_defined x.node) x.info;
      let topt, teopt = type_opt env t eopt in
      if not (subtype topt t) then type_error i.info t topt;
      mk (Idecl (t, mk x.node t, teopt)) t,
        Env.add x.node t env,
        false
  | Iif (e, i1, i2) ->
      let te = type_expr env e in
      if te.info <> Tboolean then
        type_error e.info Tboolean te.info
      else
        let ti1, _, b1 = type_instr ret env i1 in
        let ti2, _, b2 = type_instr ret env i2 in
        mk (Iif (te, ti1, ti2)) Tvoid, env, b1 && b2
  | Ifor (oe1, oe2, oe3, i') ->
      let _, toe1 = type_opt env Tvoid oe1 in
      let t2, toe2 = type_opt env Tboolean oe2 in
      if not (compatible t2 Tboolean) then type_error i.info Tboolean t2;
      let _, toe3 = type_opt env Tvoid oe3 in
      let ti, _, b = type_instr ret env i' in
      mk (Ifor (toe1, toe2, toe3, ti)) Tvoid,
      env,
      (b && oe2 = None)
  (* petit rafinement: si on sait qu'on est dans une boucle
     infinie et que le corp de la boucle a un return, on
     renvoie vrai *)
  | Iblock li ->
      let tli, _, b = List.fold_left (fun (ai, ae, b) i ->
        let ti, aee, bb = type_instr ret ae i in
        ti :: ai, aee, b || bb) ([], env, false) li
      in
      mk (Iblock (List.rev tli)) Tvoid, env, b
  | Ireturn oe ->
      let te, toe = type_opt env Tvoid oe in
      if compatible te ret then
        mk (Ireturn toe) te, env, true
      else error (Invalid_return ret) i.info

let enter_params l env =
  let tparams, nenv =
    List.fold_left (fun (acct, acce) (t,x) ->
      ((t, mk x.node t)::acct,
       Env.add x.node t acce)
    ) ([], env) l
  in
  List.rev tparams, nenv


let type_class (this, super, dlist) =
  let env = Env.add "this" (Tclass this.node) Env.empty in
  let tdlist =
    List.map (fun d ->
      match d with
      | Dfield (typ, x) ->
          Dfield (typ, mk x.node typ)
      | Dmeth (rtyp, name, params, body) ->
          let tparams, nenv = enter_params params env in
          let tbody, _, b = type_instr rtyp nenv body in
          if not b && rtyp != Tvoid then
            error Missing_return name.info
          else
            Dmeth (rtyp, mk name.node rtyp, tparams, tbody)
      | Dconstr (name, params, body) ->
          let tparams, nenv = enter_params params env in
          let tbody, _, _ = type_instr Tvoid nenv body in
          Dconstr(mk name.node Tvoid, tparams, tbody)
    ) dlist
  in
  (mk this.node (Tclass this.node),
   mk super.node (Tclass super.node),
   tdlist)

let prog (class_list, main_class, main_body) =
  let class_list = init_class_table class_list in
  if Hashtbl.mem class_table main_class then
    error (Class_redefinition main_class) (main_body.info);
  (* init_class_table a vérifié la bonne formation des attributs de
   classe et des paramètres de méthodes/constructeurs.  *)
  let tclass_list = List.map type_class class_list in
  let tmain_body, _, _ = type_instr Tvoid Env.empty main_body in
  tclass_list, main_class, tmain_body
