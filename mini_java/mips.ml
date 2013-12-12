open Format

type register =  string
let v0 : register = "$v0"
let v1 : register = "$v1"
let a0 : register = "$a0"
let a1 : register = "$a1"
let a2 : register = "$a2"
let a3 : register = "$a3"
let t0 : register = "$t0"
let t1 : register = "$t1"
let t2 : register = "$t2"
let t3 : register = "$t3"
let s0 : register = "$s0"
let s1 : register = "$s1"
let ra : register = "$ra"
let sp : register = "$sp"
let fp : register = "$fp"
let gp : register = "$gp"
let zero : register = "$zero"



type label = string
type 'a address = formatter -> 'a -> unit
let alab : label address = fun fmt  (s : label) -> fprintf fmt "%s" s
let areg : (int * register) address = fun fmt (x, y) -> fprintf fmt "%i(%s)" x y
type 'a operand = formatter -> 'a -> unit
let oreg : register operand = fun fmt (r : register) -> fprintf fmt "%s" r
let oi : int operand = fun fmt i -> fprintf fmt "%i" i
let oi32 : int32 operand = fun fmt i -> fprintf fmt "%li" i

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

type text = [`text ] asm
type data = [`data ] asm


let buf = Buffer.create 17
let fmt = formatter_of_buffer buf

let ins x =
  kfprintf (fun fmt ->
    fprintf fmt "%s" "\n" ;
    pp_print_flush fmt () ;
    let s = Buffer.contents buf in
    Buffer.clear buf; S  ("\t" ^ s)
  ) fmt x

let pr_list fmt pr l =
  match l with
    [] -> ()
  | [ i ] -> pr fmt i
  | i :: ll -> pr fmt i;
      List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

let abs = ins "abs %s, %s"
let add a b (o : 'a operand) = ins "add %s, %s, %a" a b o
let clz = ins "clz %s, %s"
let and_ = ins "and %s, %s, %s"
let div a b (o : 'a operand) = ins "div %s, %s, %a" a b o
let mul a b (o : 'a operand) = ins "mul %s, %s, %a" a b o
let or_ = ins "or %s, %s, %s"
let not_ = ins "not %s, %s"
let rem a b (o : 'a operand) = ins "rem %s, %s, %a" a b o
let neg = ins "neg %s, %s"
let sub a b (o : 'a operand) = ins "sub %s, %s, %a" a b o
let li = ins "li %s, %i"
let li32 = ins "li %s, %li"
let seq = ins "seq %s, %s, %s"
let sge = ins "sge %s, %s, %s"
let sgt = ins "sgt %s, %s, %s"
let sle = ins "sle %s, %s, %s"
let slt = ins "slt %s, %s, %s"
let sne = ins "sne %s, %s, %s"
let b (z : label) = ins "b %s" z
let beq x y (z : label) = ins "beq %s, %s, %s" x y z
let bne x y (z : label) = ins "bne %s, %s, %s" x y z
let bge x y (z : label) = ins "bge %s, %s, %s" x y z
let bgt x y (z : label) = ins "bgt %s, %s, %s" x y z
let ble x y (z : label) = ins "ble %s, %s, %s" x y z
let blt x y (z : label) = ins "blt %s, %s, %s" x y z

let beqz x (z : label) = ins "beqz %s, %s" x z
let bnez x (z : label) = ins "bnez %s, %s" x z
let bgez x (z : label) = ins "bgez %s, %s" x z
let bgtz x (z : label) = ins "bgtz %s, %s" x z
let blez x (z : label) = ins "blez %s, %s" x z
let bltz x (z : label) = ins "bltz %s, %s" x z

let jr = ins "jr %s"
let jal (z : label) = ins "jal %s" z
let jalr (z : register) = ins "jalr %s" z
let la x (p : 'a address)  = ins "la %s, %a" x p
let lb x (p : 'a address) = ins "lb %s, %a" x p
let lbu x (p : 'a address) = ins "lbu %s, %a" x p
let lw x (p : 'a address) = ins "lw %s, %a" x p
let sb x (p : 'a address) = ins "sb %s, %a" x p
let sw x (p : 'a address) = ins "sw %s, %a" x p
let move = ins "move %s, %s"
let nop = Nop
let label (s : label) = S ( s ^ ":\n")
let syscall = S  "\tsyscall\n"
let comment s = S ("#" ^ s ^ "\n")
let align = ins ".align %i"
let asciiz = ins ".asciiz %S"
let dword = ins ".word %a" pr_ilist
let address = ins ".word %a" pr_alist
let (@@) x y = Cat (x, y)


let push r =
  sub sp sp oi 4
  @@ sw r areg (0,sp)

let peek r =
  lw r areg (0,sp)

let pop r =
  peek r
  @@ add sp sp oi 4



type program = { text : [ `text ] asm;
                 data : [ `data ] asm; }

let rec pr_asm fmt a =
  match a with
  | Nop -> ()
  | S s -> fprintf fmt "%s" s
  | Cat (a1, a2) ->
      let () = pr_asm fmt a1 in
      pr_asm fmt a2

let print_program fmt p =
  fprintf fmt ".text\n";
  pr_asm fmt p.text;
  fprintf fmt ".data\n";
  pr_asm fmt p.data

(*

type data =
    | Dlabel of string
    | Dalign of int
    | Dasciiz of string
    | Dword of int32 list
    | Dbyte of int
    | Dspace of int
    | Daddress of string

type code =
    | Clist of instruction list
    | Capp of code * code

let nop = Clist []

let mips l = Clist l

let inline s = Clist [Inline s]

let (++) c1 c2 = Capp (c1, c2)

type program = {
    text : code;
    data : data list;
}

open Format

let print_register fmt = function
    | A0 -> pp_print_string fmt "$a0"
    | A1 -> pp_print_string fmt "$a1"
    | V0 -> pp_print_string fmt "$v0"
    | T0 -> pp_print_string fmt "$t0"
    | T1 -> pp_print_string fmt "$t1"
    | T2 -> pp_print_string fmt "$t2"
    | T3 -> pp_print_string fmt "$t3"
    | T4 -> pp_print_string fmt "$t4"
    | S0 -> pp_print_string fmt "$s0"
    | RA -> pp_print_string fmt "$ra"
    | SP -> pp_print_string fmt "$sp"
    | FP -> pp_print_string fmt "$fp"
    | ZERO -> pp_print_string fmt "$zero"

let print_op fmt = function
    | And -> pp_print_string fmt "and"
    | Or -> pp_print_string fmt "or"
    | Add -> pp_print_string fmt "add"
    | Sub -> pp_print_string fmt "sub"
    | Mul -> pp_print_string fmt "mul"
    | Div -> pp_print_string fmt "div"
    | Rem -> pp_print_string fmt "rem"
    | Eq -> pp_print_string fmt "seq"
    | Ne -> pp_print_string fmt "sne"
    | Lt -> pp_print_string fmt "slt"
    | Le -> pp_print_string fmt "sle"
    | Gt -> pp_print_string fmt "sgt"
    | Ge -> pp_print_string fmt "sge"

let print_address fmt = function
    | Alab s -> pp_print_string fmt s
    | Areg (ofs, r) -> fprintf fmt "%d(%a)" ofs print_register r

let print_operand fmt = function
    | Oimm i -> pp_print_int fmt i
    | Oreg r -> print_register fmt r

let print_instruction fmt = function
    | Move (dst, src) ->
	fprintf fmt "\tmove %a, %a\n" print_register dst print_register src
    | Li (r, i) ->
	fprintf fmt "\tli   %a, %d\n" print_register r i
    | Li32 (r, i) ->
	fprintf fmt "\tli   %a, %ld\n" print_register r i
    | La (r, s) ->
	fprintf fmt "\tla   %a, %s\n" print_register r s
    | Lw (r, a) ->
	fprintf fmt "\tlw   %a, %a\n" print_register r print_address a
    | Sw (r, a) ->
	fprintf fmt "\tsw   %a, %a\n" print_register r print_address a
    | Lbu (r, a) ->
	fprintf fmt "\tlbu   %a, %a\n" print_register r print_address a
    | Sb (r, a) ->
	fprintf fmt "\tsb   %a, %a\n" print_register r print_address a
    | Binop (o, dst, src, op) ->
	fprintf fmt "\t%a  %a, %a, %a\n"
	    print_op o print_register dst print_register src print_operand op
    | Neg (dst, src) ->
	fprintf fmt "\tneg  %a, %a\n" print_register dst print_register src
    | B l ->
	fprintf fmt "\tb    %s\n" l
    | Beqz (r, l) ->
	fprintf fmt "\tbeqz %a, %s\n" print_register r l
    | Bnez (r, l) ->
	fprintf fmt "\tbnez %a, %s\n" print_register r l
    | Jal s ->
	fprintf fmt "\tjal  %s\n" s
    | Jalr r ->
	fprintf fmt "\tjalr %a\n" print_register r
    | Jr r ->
	fprintf fmt "\tjr   %a\n" print_register r
    | Syscall ->
	fprintf fmt "\tsyscall\n"
    | Label s ->
	fprintf fmt "%s:\n" s
    | Inline s ->
	fprintf fmt "%s" s
    | Comment s ->
	fprintf fmt "#%s\n" s

let rec print_code fmt = function
    | Clist l -> List.iter (print_instruction fmt) l
    | Capp (c1, c2) -> print_code fmt c1; print_code fmt c2


let print_list sep pre fmt l =
    match l with
	[] -> ()
    | [ e ] -> fprintf fmt "%a" pre e
    | e :: l ->
	fprintf fmt "%a" pre e;
	List.iter (fun e -> fprintf fmt "; %a" pre e) l;
	fprintf fmt "%!"

let print_data fmt = function
    | Dlabel s ->
	fprintf fmt "%s:\n" s
    | Dalign i ->
	fprintf fmt "\t.align %i\n" i
    | Dasciiz s ->
	fprintf fmt "\t.asciiz %S\n" s
    | Dword n ->
	fprintf fmt "\t.word %a\n" (print_list "," (fun fmt i -> fprintf fmt "%li" i)) n
    | Dbyte n ->
	fprintf fmt "\t.byte %i\n" (n land 0xff)
    | Dspace n ->
	fprintf fmt "\t.space %d\n" n
    | Daddress s ->
	fprintf fmt "\t.word %s\n" s

let print_program fmt p =
    fprintf fmt "\t.text\n";
    print_code fmt p.text;
    fprintf fmt "\t.data\n";
    List.iter (print_data fmt) p.data;
    fprintf fmt "@."
*)
