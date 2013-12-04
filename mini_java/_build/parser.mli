type token =
  | INTEGER of (Int32.t)
  | STRING of (string)
  | BOOL of (bool)
  | IDENT of (string)
  | BOOLEAN
  | CLASS
  | ELSE
  | EXTENDS
  | FOR
  | IF
  | INSTANCEOF
  | INT
  | NEW
  | NULL
  | PUBLIC
  | RETURN
  | STATIC
  | THIS
  | VOID
  | EQ
  | EQEQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | PLUSPLUS
  | MINUSMINUS
  | NOT
  | DOT
  | OR
  | AND
  | BANG
  | LP
  | RP
  | LB
  | RB
  | EOF
  | COMMA
  | SEMICOLON
  | LSB
  | RSB

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.parsed_prog
