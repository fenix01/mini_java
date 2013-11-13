open Ast

module MethodSignature :
  sig type t = string * typ list val compare : t -> t -> int end

module MethodSet : Set.S with type elt = MethodSignature.t
module MethodMap : Map.S with type key = MethodSignature.t

type method_desc = {
  return : typ;
  class_def : string;
  class_override : string;
}

val mangle : string -> string -> string -> typ list -> string

type class_desc = {
  name : string;
  parent : string;
  mutable fields : (string * (typ * string)) list;
  mutable methods : method_desc MethodMap.t;
  mutable ctors : method_desc MethodMap.t;
}

val class_table : (string, class_desc) Hashtbl.t
val subclass : string -> string -> bool
val subtype : typ -> typ -> bool
val compatible : typ -> typ -> bool
val wf : typ -> bool
val check_wf : typ -> position -> unit

val select_field : string -> position ident -> typ * string
val select_method :
  string -> position ident -> typ list -> string * typ list * typ * string
val select_constr :
  position ident -> typ list -> string * typ list * typ * string

val init_class_table :
  (position ident * position ident * position class_decl list) list
  ->
  (position ident * position ident * position class_decl list) list
