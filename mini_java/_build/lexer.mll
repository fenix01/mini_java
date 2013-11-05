(* Analyse lexicale *)
{

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
}
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | '_') (alpha | '_' | digit)*
let char = ([' ' - '~'] # [ '\\' '\'' '\"']) | '\\' ('n' | 't' | '\'' |'\"')

rule token = parse
  | '\n'
      { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | "//" [^'\n']* ('\n' | eof)
      { Lexing.new_line lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | digit+ as s
      {
	try
	  INTEGER (Int32.of_string s)
	with _ ->
	  raise (Lexical_error ("invalid integer constant '" ^ s ^ "'"))
      }
  | '\"'
      { Buffer.reset str_buff;
        string lexbuf }

  | '(' { LP }
  | ')' { RP }
  | '{' { LB }
  | '}' { RB }
  | '['
      { LSB }
  | ']'
      { RSB }
  | ','
      { COMMA }
  | ';'
      { SEMICOLON }
  | '.'
      { DOT }
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { TIMES }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "!"
      { BANG }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "="
      { EQ }
  | ">"
      { GT }
  | ">="
      { GEQ }
  | "<"
      { LT }
  | "<="
      { LEQ }
  | "=="
      { EQEQ }
  | "!="
      { NEQ }
  | "++"
      { PLUSPLUS }
  | "--"
      { MINUSMINUS }
  | eof
      { EOF }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
  | "*/" { () }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
  | _    { comment lexbuf }

and string = parse
  | char as c { Buffer.add_char str_buff (decode_char c);
                string lexbuf }
  | '\"' { STRING (Buffer.contents str_buff) }
  | eof  { raise (Lexical_error "unterminated string") }
  | _ as c { raise (Lexical_error ("invalid character " ^ (String.make 1 c))) }
