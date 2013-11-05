(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Type_class
open Typing

exception InvalidPublicClass of string

let ext = ".java"
let usage = sprintf "usage: %s [options] file%s" Sys.argv.(0) ext

let parse_only = ref false
let type_only = ref false


let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
(*   "-type-only", Arg.Set type_only, "  stops after typing"; *)
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ext) then
      raise (Arg.Bad "invalid extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  if b = dummy_pos || e = dummy_pos then
  eprintf "File \"%s\"\nerror: " file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d\nerror: " file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let (_, main, _) as p = Parser.prog Lexer.token lb in
    if main ^ ext <> Filename.basename file then  raise (InvalidPublicClass main);
    close_in c;
    if !parse_only then exit 0;
		type_prog p;
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    |  InvalidPublicClass main ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "this file should be named %s%s\n@." main ext;
	exit 1
    |  Class_error (msg,p) ->
	report_loc p;
	eprintf "%s\n@." msg ;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
