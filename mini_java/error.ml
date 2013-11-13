open Ast
open Format
open Lexing

type error =
  | Lexical_error of string
  | Syntax_error
  | Class_redefinition of string
  | Cannot_extend_string of string
  | Cyclic_inheritance of string
  | Undefined_class of string
  | Ambiguous_method_call of string
  | No_candidate_method of string
  | Type_error of typ * typ
  | Call_on_non_class_type of string * typ
  | Invalid_addition of  typ * typ
  | Invalid_print
  | Invalid_cast of typ * typ
  | Invalid_instanceof of typ * typ
  | Not_lvalue
  | Unbound_identifier of string
  | Invalid_field_access of string
  | Invalid_return of typ
  | Invalid_type of typ
  | Already_defined of string
  | This_in_static
  | Redefined_attribute of string * string
  | Redefined_method of string * string
  | Redefined_constructor of string
  | Invalid_constructor of string * string
  | Invalid_override of string * string * string * typ * typ
  | Invalid_public_class of string
  | Missing_return

exception Error of error * Ast.position


let report_loc fmt file (b,e) =
  if b = dummy_pos || e = dummy_pos then
  fprintf fmt "File \"%s\"\nerror: " file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf fmt "File \"%s\", line %d, characters %d-%d\nerror: " file l fc lc

let typ_to_string t =
  match t with
    Tvoid -> "void"
  | Tint -> "int"
  | Tboolean -> "boolean"
  | Tnull -> "null"
  | Tclass s -> s

let to_string e =
  match e with
    Lexical_error s -> sprintf "lexical error: %s" s
  | Syntax_error -> "syntax error"
  | Class_redefinition s -> sprintf "class %s is already defined" s
  | Cannot_extend_string s -> sprintf "class %s cannot extend String" s
  | Cyclic_inheritance s -> sprintf "cycle detected in the hierarchy of class %s" s
  | Undefined_class s -> sprintf "class %s is undefined" s
  | Ambiguous_method_call s -> sprintf "invocation of method %s is ambiguous" s
  | No_candidate_method s -> sprintf "invocation of method %s has no candidate" s
  | Type_error (t1, t2) -> sprintf "this expression has type %s but should have type %s" (typ_to_string t1) (typ_to_string t2)
  | Call_on_non_class_type (m, t) -> sprintf "invocation of method %s on non class type %s" m (typ_to_string t)
  | Invalid_addition (t1, t2) -> sprintf "invalid types %s and %s for operator +" (typ_to_string t1) (typ_to_string t2)
  | Invalid_print -> "invalid type for System.out.print()"
  | Invalid_cast (t1, t2) -> sprintf "invalid cast between type %s and %s" (typ_to_string t1) (typ_to_string t2)
  | Invalid_instanceof (t1, t2) -> sprintf "invalid instanceof between type %s and %s" (typ_to_string t1) (typ_to_string t2)
  | Not_lvalue -> "expression must be an lvalue"
  | Unbound_identifier s -> sprintf "unbound identifier %s" s
  | Invalid_field_access s -> sprintf "invalid field access %s" s
  | Invalid_return t -> sprintf "invalid return type %s" (typ_to_string t)
  | Invalid_type t -> sprintf "invalid type %s" (typ_to_string t)
  | Already_defined s -> sprintf "identifier %s is already defined" s
  | This_in_static -> "invalid use of this in static method"
  | Redefined_method (c,s) -> sprintf "redefinition of method %s in class %s" s c

  | Redefined_attribute (c,s) -> sprintf "redefinition of attribute %s in class %s" s c
  | Redefined_constructor c -> sprintf "redefinition of constructor in class %s" c
  | Invalid_constructor (c,s) -> sprintf "invalid name for constructor '%s' in class %s" s c
  | Invalid_override (c1, f1, c2, t1, t2) ->
      sprintf "method %s in class %s cannot override %s in class %s. Return type %s is not compatible with %s"
        f1 c1 f1 c2 (typ_to_string t1) (typ_to_string t2)
  | Invalid_public_class s -> sprintf "invalid name for public class %s" s
  | Missing_return -> "missing return statement"

let print fmt f e p =
  report_loc fmt f p;
  fprintf fmt "%s\n@." (to_string e)

let error e p = raise (Error (e,p))
