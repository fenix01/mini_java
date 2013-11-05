# 2 "lexer.mll"
 

  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ "boolean", BOOLEAN;
        "class", CLASS;
        "else", ELSE;
        "extends", EXTENDS;
        "false", BOOL false;
        "for", FOR;
        "if", IF;
        "instanceof", INSTANCEOF;
        "int", INT;
        "new", NEW;
        "null", NULL;
        "public", PUBLIC;
        "return", RETURN;
        "static", STATIC;
        "this", THIS;
        "true", BOOL true;
        "void", VOID;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let decode_char s =
    if String.length s = 1 then s.[0] else
      if String.length s = 2 then
        match s.[1] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | '\'' -> '\''
        | '\"' -> '\"'
        | _ -> raise (Lexical_error ("invalid escape sequence " ^ s))
      else assert false

  let str_buff = Buffer.create 256

# 49 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\221\255\222\255\002\000\003\000\031\000\002\000\001\000\
    \033\000\235\255\237\255\015\000\051\000\240\255\241\255\242\255\
    \243\255\244\255\245\255\246\255\247\255\248\255\249\255\079\000\
    \089\000\105\000\003\000\255\255\001\000\253\255\252\255\223\255\
    \224\255\225\255\233\255\232\255\226\255\229\255\227\255\138\000\
    \252\255\253\255\254\255\102\000\255\255\226\000\252\255\253\255\
    \254\255\063\001\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\027\000\025\000\024\000\034\000\034\000\
    \021\000\255\255\255\255\017\000\016\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\005\000\
    \004\000\019\000\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
    \000\000\000\000\000\000\255\255\000\000\046\000\000\000\000\000\
    \000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\026\000\027\000\030\000\026\000\026\000\000\000\000\000\
    \026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \026\000\008\000\022\000\026\000\000\000\009\000\007\000\034\000\
    \021\000\020\000\010\000\011\000\015\000\012\000\013\000\025\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\032\000\014\000\003\000\005\000\004\000\038\000\
    \037\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\017\000\036\000\016\000\033\000\024\000\
    \031\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\019\000\006\000\018\000\035\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\029\000\042\000\044\000\000\000\000\000\
    \028\000\000\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\043\000\000\000\000\000\000\000\
    \024\000\000\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\030\000\050\000\050\000\048\000\050\000\050\000\050\000\
    \050\000\000\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\049\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\000\000\000\000\000\000\000\000\050\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\050\000\000\000\000\000\
    \000\000\000\000\000\000\050\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\047\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\028\000\026\000\000\000\255\255\255\255\
    \026\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\026\000\255\255\000\000\000\000\007\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\011\000\000\000\000\000\000\000\000\000\003\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\000\000\008\000\000\000\
    \012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\006\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\025\000\039\000\043\000\255\255\255\255\
    \025\000\255\255\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\039\000\255\255\255\255\255\255\
    \024\000\255\255\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\028\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\255\255\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\039\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
    \255\255\255\255\255\255\049\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\045\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 55 "lexer.mll"
      ( Lexing.new_line lexbuf; token lexbuf )
# 244 "lexer.ml"

  | 1 ->
# 57 "lexer.mll"
      ( token lexbuf )
# 249 "lexer.ml"

  | 2 ->
# 59 "lexer.mll"
      ( comment lexbuf; token lexbuf )
# 254 "lexer.ml"

  | 3 ->
# 61 "lexer.mll"
      ( Lexing.new_line lexbuf; token lexbuf )
# 259 "lexer.ml"

  | 4 ->
# 63 "lexer.mll"
      ( id_or_keyword (lexeme lexbuf) )
# 264 "lexer.ml"

  | 5 ->
let
# 64 "lexer.mll"
              s
# 270 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 65 "lexer.mll"
      (
	try
	  INTEGER (Int32.of_string s)
	with _ ->
	  raise (Lexical_error ("invalid integer constant '" ^ s ^ "'"))
      )
# 279 "lexer.ml"

  | 6 ->
# 72 "lexer.mll"
      ( Buffer.reset str_buff;
        string lexbuf )
# 285 "lexer.ml"

  | 7 ->
# 75 "lexer.mll"
        ( LP )
# 290 "lexer.ml"

  | 8 ->
# 76 "lexer.mll"
        ( RP )
# 295 "lexer.ml"

  | 9 ->
# 77 "lexer.mll"
        ( LB )
# 300 "lexer.ml"

  | 10 ->
# 78 "lexer.mll"
        ( RB )
# 305 "lexer.ml"

  | 11 ->
# 80 "lexer.mll"
      ( LSB )
# 310 "lexer.ml"

  | 12 ->
# 82 "lexer.mll"
      ( RSB )
# 315 "lexer.ml"

  | 13 ->
# 84 "lexer.mll"
      ( COMMA )
# 320 "lexer.ml"

  | 14 ->
# 86 "lexer.mll"
      ( SEMICOLON )
# 325 "lexer.ml"

  | 15 ->
# 88 "lexer.mll"
      ( DOT )
# 330 "lexer.ml"

  | 16 ->
# 90 "lexer.mll"
      ( MINUS )
# 335 "lexer.ml"

  | 17 ->
# 92 "lexer.mll"
      ( PLUS )
# 340 "lexer.ml"

  | 18 ->
# 94 "lexer.mll"
      ( TIMES )
# 345 "lexer.ml"

  | 19 ->
# 96 "lexer.mll"
      ( DIV )
# 350 "lexer.ml"

  | 20 ->
# 98 "lexer.mll"
      ( MOD )
# 355 "lexer.ml"

  | 21 ->
# 100 "lexer.mll"
      ( BANG )
# 360 "lexer.ml"

  | 22 ->
# 102 "lexer.mll"
      ( AND )
# 365 "lexer.ml"

  | 23 ->
# 104 "lexer.mll"
      ( OR )
# 370 "lexer.ml"

  | 24 ->
# 106 "lexer.mll"
      ( EQ )
# 375 "lexer.ml"

  | 25 ->
# 108 "lexer.mll"
      ( GT )
# 380 "lexer.ml"

  | 26 ->
# 110 "lexer.mll"
      ( GEQ )
# 385 "lexer.ml"

  | 27 ->
# 112 "lexer.mll"
      ( LT )
# 390 "lexer.ml"

  | 28 ->
# 114 "lexer.mll"
      ( LEQ )
# 395 "lexer.ml"

  | 29 ->
# 116 "lexer.mll"
      ( EQEQ )
# 400 "lexer.ml"

  | 30 ->
# 118 "lexer.mll"
      ( NEQ )
# 405 "lexer.ml"

  | 31 ->
# 120 "lexer.mll"
      ( PLUSPLUS )
# 410 "lexer.ml"

  | 32 ->
# 122 "lexer.mll"
      ( MINUSMINUS )
# 415 "lexer.ml"

  | 33 ->
# 124 "lexer.mll"
      ( EOF )
# 420 "lexer.ml"

  | 34 ->
# 126 "lexer.mll"
      ( raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) )
# 425 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 39
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 129 "lexer.mll"
         ( () )
# 436 "lexer.ml"

  | 1 ->
# 130 "lexer.mll"
         ( Lexing.new_line lexbuf; comment lexbuf )
# 441 "lexer.ml"

  | 2 ->
# 131 "lexer.mll"
         ( raise (Lexical_error "unterminated comment") )
# 446 "lexer.ml"

  | 3 ->
# 132 "lexer.mll"
         ( comment lexbuf )
# 451 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 45
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 135 "lexer.mll"
            c
# 463 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 135 "lexer.mll"
              ( Buffer.add_char str_buff (decode_char c);
                string lexbuf )
# 468 "lexer.ml"

  | 1 ->
# 137 "lexer.mll"
         ( STRING (Buffer.contents str_buff) )
# 473 "lexer.ml"

  | 2 ->
# 138 "lexer.mll"
         ( raise (Lexical_error "unterminated string") )
# 478 "lexer.ml"

  | 3 ->
let
# 139 "lexer.mll"
         c
# 484 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 139 "lexer.mll"
           ( raise (Lexical_error ("invalid character " ^ (String.make 1 c))) )
# 488 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

;;
