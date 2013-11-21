(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Typing
open Compile

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

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let (_, main, body) as p = Parser.prog Lexer.token lb in
    if main ^ ext <> Filename.basename file then
      Error.error (Error.Invalid_public_class main) body.info;
    close_in c;
    if !parse_only then exit 0;
    let tast = Typing.prog p in
    if !type_only then exit 0;
    let prog = prog tast in
    let output_file = (Filename.chop_suffix file ext) ^ ".s" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mips.print_program outf prog;
    pp_print_flush outf ();
    close_out out
  with
  | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
  | Parsing.Parse_error ->
      Error.print err_formatter file Error.Syntax_error
        (Parsing.symbol_start_pos(),
         Parsing.symbol_end_pos());
      exit 1
  | e ->
      let s = Printexc.get_backtrace() in
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      eprintf "Backtrace: %s\n@." s;
      exit 2
