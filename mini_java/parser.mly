%{
  open Ast
  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let loc x = { node = x; info = current_pos () }
  let syntax_error () =
    Error.error Error.Syntax_error (current_pos())
%}
/* objet de base */
%token <Int32.t> INTEGER
%token <string> STRING
%token <bool> BOOL
%token <string> IDENT

/* Mots clés du langage */
%token BOOLEAN CLASS ELSE EXTENDS FOR IF INSTANCEOF INT
%token NEW NULL PUBLIC RETURN STATIC THIS VOID

/* Operateurs */
%token EQ EQEQ NEQ LT GT LEQ GEQ PLUS MINUS TIMES DIV MOD PLUSPLUS MINUSMINUS NOT DOT OR AND BANG

/* Délimiteurs */
%token LP RP LB RB EOF COMMA SEMICOLON LSB RSB

%nonassoc then
%nonassoc ELSE

%right EQ
%left OR
%left AND
%left EQEQ NEQ
%left LT LEQ GT GEQ INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right uminus PLUSPLUS MINUSMINUS NOT BANG cast
%nonassoc par
%left DOT

%start prog
%type <Ast.parsed_prog> prog

%%

prog:
  class_list main_class EOF { let main, instr = $2 in
                              $1, main, instr }
  | error               EOF { syntax_error () }
;

main_class:
PUBLIC CLASS IDENT LB PUBLIC STATIC VOID IDENT LP IDENT IDENT LSB RSB RP block RB
{
  if  $8 <> "main" || $10 <> "String" then syntax_error ()
  else $3, $15
}
;

class_list:
            class_list_rev { List.rev $1 }
;

class_list_rev:
|                       { [  ] }
| class_list_rev class_ { $2 :: $1 }
;

class_:
  CLASS ident maybe_extends LB decl_list RB { $2, $3, $5  }
;

maybe_extends:
                      { loc "Object" }
| EXTENDS ident       { $2       }
;


decl_list:
  decl_list_rev { List.rev $1 }
;

decl_list_rev:
                     { [] }
| decl_list_rev decl { $2 :: $1 }
;


type_expr :
  INT                { Tint }
| BOOLEAN               { Tboolean }
| IDENT              { Tclass $1 }
;

decl:
  type_expr ident SEMICOLON                            { Dfield ($1, $2) }
| ident LP param_list RP block  { Dconstr ($1, $3, $5) }
| type_expr ident LP param_list RP block { Dmeth ($1, $2, $4, $6) }
| VOID ident LP param_list RP block { Dmeth (Tvoid, $2, $4, $6) }

;

block:
LB instr_list RB         { loc (Iblock $2) }
;

param_list:
                   { [] }
| param_list_rev    { List.rev $1 }
;

param_list_rev:
  type_expr ident                { [($1, $2)] }
| param_list_rev COMMA type_expr ident { ($3, $4) :: $1 }
;

instr_list:
instr_list_rev              { List.rev $1 }
;

ident:
IDENT              { loc $1 }
;

instr_list_rev:
                     { [] }
| instr_list_rev instr { $2 :: $1 }
;

instr:
  SEMICOLON               { loc (Iblock []) }
| expr SEMICOLON          { loc (Iexpr $1) }
| type_expr ident maybe_init SEMICOLON         { loc (Idecl ($1, $2, $3)) }
| IF LP expr RP instr          %prec then     { loc (Iif ($3, $5, loc (Iblock []))) }
| IF LP expr RP instr ELSE instr               { loc (Iif ($3, $5, $7)) }
| FOR LP maybe_expr SEMICOLON maybe_expr SEMICOLON maybe_expr RP instr
                                           { loc (Ifor($3, $5, $7, $9)) }
| block                                        { $1 }
| RETURN maybe_expr SEMICOLON                  { loc (Ireturn $2) }
;

maybe_init:
                    { None }
| EQ expr              { Some $2 }
;
maybe_expr:
                    { None }
| expr              { Some $1 }
;

expr:
  INTEGER                  { loc (Econst (Cint $1)) }
| STRING               { loc (Econst (Cstring $1)) }
| BOOL                 { loc (Econst (Cbool $1)) }
| THIS                 { loc (Elval (Lident (loc "this"))) }
| NULL                 { loc (Econst Cnull) }
| LP expr RP %prec par { $2 }

| lvalue               { loc (Elval $1) }

| lvalue EQ expr       { loc (Eassign ($1, $3)) }

| lvalue LP expr_list RP { loc (Ecall ($1, $3)) }
| NEW ident LP expr_list RP { loc (Enew ($2, $4)) }
| PLUSPLUS expr          { loc (Eunop (Upre_inc, $2)) }
| MINUSMINUS expr          { loc (Eunop (Upre_dec, $2)) }
| expr PLUSPLUS         { loc (Eunop (Upost_inc, $1)) }
| expr MINUSMINUS         { loc (Eunop (Upost_dec, $1)) }
| BANG expr               { loc (Eunop (Unot, $2)) }
| MINUS expr  %prec uminus  { loc (Eunop (Uneg, $2)) }
| expr PLUS expr          { loc (Ebinop ($1, Badd, $3)) }
| expr MINUS expr          { loc (Ebinop ($1, Bsub, $3)) }
| expr TIMES expr          { loc (Ebinop ($1, Bmul, $3)) }
| expr DIV expr          { loc (Ebinop ($1, Bdiv, $3)) }
| expr MOD expr          { loc (Ebinop ($1, Bmod, $3)) }
| expr OR expr          { loc (Ebinop ($1, Bor, $3)) }
| expr AND expr          { loc (Ebinop ($1, Band, $3)) }
| expr EQEQ expr          { loc (Ebinop ($1, Beq, $3)) }
| expr NEQ expr          { loc (Ebinop ($1, Bneq, $3)) }
| expr LT expr          { loc (Ebinop ($1, Blt, $3)) }
| expr LEQ expr          { loc (Ebinop ($1, Blte, $3)) }
| expr GT expr          { loc (Ebinop ($1, Bgt, $3)) }
| expr GEQ expr          { loc (Ebinop ($1, Bgte, $3)) }
| LP INT RP expr      %prec cast  { loc (Ecast(Tint, $4)) }
| LP BOOLEAN RP expr  %prec cast { loc (Ecast(Tboolean, $4)) }
| LP expr RP expr     %prec cast  {
                         match ($2).node with
                               | Elval (Lident id) -> loc (Ecast(Tclass id.node, $4))
                               | _ -> syntax_error ()
}
| expr INSTANCEOF type_expr  { loc (Einstanceof($1, $3)) }

;

lvalue:
 ident          { Lident $1 }
| expr DOT ident { Laccess ($1, $3) }
;

expr_list:
               { [] }
| expr_list_rev { List.rev $1 }
;

expr_list_rev:
expr             { [ $1 ] }
| expr_list_rev COMMA expr { $3 :: $1 }
;
