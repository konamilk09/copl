open Syntax

let evalto = " evalto "

let indent i = String.make i ' '

let rec string_of_judg judg = match judg with
  Evalto (e,v) ->
    string_of_expr_print e ^
    " evalto " ^
    string_of_value v

let bplus s1 s2 v i = indent i ^
  s1 ^ " plus " ^
  s2 ^ " is " ^
  string_of_value v ^ " by B-Plus {};\n"

let bminus s1 s2 v i = indent i ^
  s1 ^ " minus " ^
  s2 ^ " is " ^
  string_of_value v ^ " by B-Minus{};\n"

let btimes s1 s2 v i = indent i ^
  s1 ^ " times " ^
  s2 ^ " is " ^
  string_of_value v ^ " by B-Times{};\n"

(* 式に保存されている式の評価結果を文字列で取ってくる *)
let get_value expr = match expr with
  Num (n) -> string_of_int n
| Op (_,_,_, v) -> match v with
    VNum(n) -> string_of_int n
| _ -> "" (* deref しているのでここにはこない *)

let rec g_expr expr i = match expr with
  Num (n) -> indent i ^
    string_of_int n ^
    evalto ^
    string_of_int n ^
    " by E-Int {};\n"
| Op (e1, op, e2, v) ->
  let s1 = get_value e1 in
  let s2 = get_value e2 in
  match op with
    Plus -> indent i ^
      (string_of_judg (Evalto(expr, v))) ^
      " by E-Plus {\n" ^
      g_expr e1 (i+2) ^
      g_expr e2 (i+2) ^
      bplus s1 s2 v (i+2) ^
      indent i ^ "};\n"
  | Minus -> indent i ^
      (string_of_judg (Evalto(expr, v))) ^
      " by E-Minus {\n" ^
      g_expr e1 (i+2) ^
      g_expr e2 (i+2) ^
      bminus s1 s2 v (i+2) ^
      indent i ^ "};\n"
  | Times -> indent i ^
    (string_of_judg (Evalto(expr, v))) ^
    " by E-Times {\n" ^
    g_expr e1 (i+2) ^
    g_expr e2 (i+2) ^
    btimes s1 s2 v (i+2) ^
    indent i ^ "};\n"

(* interpreter *)
let rec g judg = match judg with
Evalto (expr, v) -> g_expr expr 0

(* start point *)
(* Eval.f: Syntax.t -> string *)
let rec f judg = g judg
