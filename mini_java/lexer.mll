(* Analyse lexicale *)
{

  open Lexing
  open Parser
  open Ast
  open Error
  open Format

  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

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


  let str_buff = Buffer.create 256
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | '_') (alpha | '_' | digit)*
let char = ([' ' - '~'] # [ '\\' '\'' '\"']) | '\\' ('n' | 't' | '\\' |'\"')

rule token = parse
  | '\n'
      { new_line lexbuf; token lexbuf }
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
          error (Lexical_error
                   (sprintf "invalid integer constant '%s'" s))
            (current_pos lexbuf)
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
      { error (Lexical_error ("illegal character: " ^ lexeme lexbuf))
          (current_pos lexbuf)
      }

and comment = parse
  | "*/" { () }
  | '\n' { new_line lexbuf; comment lexbuf }
  | eof  { error (Lexical_error ("unterminated comment"))  (current_pos lexbuf)}
  | _    { comment lexbuf }

and string = parse
  | char as s {
    let c =
      if String.length s = 1 then s.[0] else
        match s.[1] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | '\\' -> '\\'
        | '\"' -> '\"'
        | _ ->
            error
              (Lexical_error ("invalid escape sequence " ^ s))
              (current_pos lexbuf)
    in
    Buffer.add_char str_buff c;
    string lexbuf }
  | '\"' { STRING (Buffer.contents str_buff) }
  | eof  { error (Lexical_error ("unterminated string"))  (current_pos lexbuf)}
  | _ as c { error (Lexical_error (sprintf "invalid character '%c'" c)) (current_pos lexbuf) }
