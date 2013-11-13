type ('info, 'data) node = { node : 'data;
                             info : 'info; }

type 'info ident = ('info, string) node

type const =
    Cint of Int32.t
  | Cstring of string
  | Cbool of bool
  | Cnull

type typ =
    Tboolean
  | Tint
  | Tclass of string
  | Tvoid
  | Tnull

type unop = Upost_inc | Upost_dec | Upre_inc | Upre_dec | Unot | Uneg
type binop = Beq | Bneq | Blt | Blte | Bgt | Bgte
             | Badd | Bsub | Bmul | Bdiv | Bmod
             | Band | Bor

type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
    Econst of const
  | Elval of 'info lvalue
  | Eassign of 'info lvalue * 'info expr
  | Ecall of 'info lvalue * 'info expr list
  | Enew of 'info ident * 'info expr list
  | Eunop of unop * 'info expr
  | Ebinop of 'info expr * binop * 'info expr
  | Ecast of typ * 'info expr
  | Einstanceof of 'info expr * typ

and 'info lvalue =
    Lident of 'info ident
  | Laccess of 'info expr * 'info ident

type 'info instr = ('info, 'info instr_node) node
and 'info instr_node =
    Iexpr of 'info expr
  | Idecl of typ * 'info ident * ('info expr option)
  | Iif of 'info expr * 'info instr * 'info instr
  | Ifor of 'info expr option * 'info expr option * 'info expr option * 'info instr
  | Iblock of ('info instr) list
  | Ireturn of 'info expr option

type 'info class_decl =
    Dfield of typ * 'info ident
  | Dconstr of 'info ident * (typ * 'info ident) list * 'info instr
  | Dmeth of typ * 'info ident * (typ * 'info ident) list * 'info instr

type 'info klass = 'info ident * 'info ident * 'info class_decl list

type 'info prog = 'info klass list * string * 'info instr

type position = Lexing.position * Lexing.position

type parsed_prog = position prog
type typed_prog = typ prog
