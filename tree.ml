open Syntax

let evalto = " evalto "

let indent i = String.make i ' '

let rec string_of_judg judg = match judg with
  Evalto (e,v) -> string_of_expr_print e
    ^ " evalto "
    ^ string_of_value v

let bplus s1 s2 v i = indent i
    ^ string_of_value s1 ^ " plus "
    ^ string_of_value s2 ^ " is "
    ^ string_of_value v ^ " by B-Plus {};\n"

let bminus s1 s2 v i = indent i
    ^ string_of_value s1 ^ " minus "
    ^ string_of_value s2 ^ " is "
    ^ string_of_value v ^ " by B-Minus{};\n"

let btimes s1 s2 v i = indent i
    ^ string_of_value s1 ^ " times "
    ^ string_of_value s2 ^ " is "
    ^ string_of_value v ^ " by B-Times{};\n"

let bless s1 s2 v i = indent i
    ^ string_of_value s1 ^ " less than "
    ^ string_of_value s2 ^ " is "
    ^ string_of_value v ^ " by B-Lt{};\n"

(* 式に保存されている式の評価結果を値として取ってくる *)
let get_value expr = match expr with
  Num (n) -> VNum(n)
| Bool (b) -> VBool(b)
| Op (_,_,_, v) -> v
| If(_,_,_, v) -> v

let rec g_expr expr i = match expr with
  Num (n) -> indent i
    ^ string_of_int n
    ^ evalto
    ^ string_of_int n
    ^ " by E-Int {};\n"
| Bool (b) -> indent i
    ^ string_of_bool b
    ^ evalto
    ^ string_of_bool b
    ^ " by E-Bool {};\n"
| Op (e1, op, e2, v) -> (
  let s1 = get_value e1 in
  let s2 = get_value e2 in
  match op with
    Plus -> indent i
      ^ (string_of_judg (Evalto(expr, v)))
      ^ " by E-Plus {\n"
      ^ g_expr e1 (i+2)
      ^ g_expr e2 (i+2)
      ^ bplus s1 s2 v (i+2)
      ^ indent i ^ "};\n"
  | Minus -> indent i
      ^ (string_of_judg (Evalto(expr, v)))
      ^ " by E-Minus {\n"
      ^ g_expr e1 (i+2)
      ^ g_expr e2 (i+2)
      ^ bminus s1 s2 v (i+2)
      ^ indent i ^ "};\n"
  | Times -> indent i
      ^ (string_of_judg (Evalto(expr, v)))
      ^ " by E-Times {\n"
      ^ g_expr e1 (i+2)
      ^ g_expr e2 (i+2)
      ^ btimes s1 s2 v (i+2)
      ^ indent i ^ "};\n"
  | Less -> indent i
      ^ (string_of_judg (Evalto(expr, v)))
      ^ " by E-Lt {\n"
      ^ g_expr e1 (i+2)
      ^ g_expr e2 (i+2)
      ^ bless s1 s2 v (i+2)
      ^ indent i ^ "};\n")
| If (e1, e2, e3, v) ->
  let s1 = get_value e1 in
  if s1 = VBool(true) then indent i
    ^ (string_of_judg (Evalto(expr, v)))
    ^ " by E-IfT {\n"
    ^ g_expr e1 (i+2)
    ^ g_expr e2 (i+2)
    ^ indent i ^ "};\n"
  else indent i
    ^ (string_of_judg (Evalto(expr, v)))
    ^ " by E-IfF {\n"
    ^ g_expr e1 (i+2)
    ^ g_expr e3 (i+2)
    ^ indent i ^ "};\n"

(* interpreter *)
let rec g judg = match judg with
Evalto (expr, v) -> g_expr expr 0

(* start point *)
(* Eval.f: Syntax.t -> string *)
let rec f judg = g judg
