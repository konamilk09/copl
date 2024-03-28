open Syntax

(* デバッグ用 print *)
let rec string_of_expr_eval expr = match expr with
  Num (Int(i)) -> string_of_int i
| Op (e1,op,e2,v) -> begin match op with
    Plus -> "(" ^
    string_of_expr_eval e1 ^
    "+" ^
    string_of_expr_eval e2 ^
    "[" ^ string_of_value v ^ "]" ^
    ")"
  | Times -> "(" ^
    string_of_expr_eval e1 ^
    "*" ^
    string_of_expr_eval e2 ^
    "[" ^ string_of_value v ^ "]" ^
    ")"
  | Minus -> "(" ^
    string_of_expr_eval e1 ^
    "-" ^
    string_of_expr_eval e2 ^
    "[" ^ string_of_value v ^ "]" ^
    ")"
end
(* print the AST *)
let rec string_of_judg_eval judg = match judg with
  Evalto (e,v) ->
    string_of_expr_eval e
    ^ " evalto "
    ^ string_of_value v

let print_eval judg =
  let str = string_of_judg_eval judg
in print_string str

(* VVAr と具体的な値を受け取って、VVar に値をセットする *)
(* set: VVar -> int -> unit *)
let set v i = match v with
  VNum(Int(i)) -> ()
| VVar(v) -> v := Some(Syntax.VNum(Syntax.Int(i)))

(* deref のときにまだ評価されていない変数があったらエラー *)
exception UnEval
let rec deref_value v = match v with
  VNum (Int(i)) -> v
| VVar ({contents=None}) -> raise UnEval
| VVar ({contents=Some(VNum(Int(i)))}) -> VNum(Int(i))
| VVar ({contents=Some(VVar(_) as vv)}) -> deref_value vv
let rec deref_expr expr = match expr with
  Num (Int(i)) -> expr
| Op (e1,op,e2,v) ->
    Op(deref_expr e1, op, deref_expr e2, deref_value v)
(* これいらない？ *)
let deref judg = match judg with
  Evalto (expr,v) -> Evalto (deref_expr expr,v)

let rec g_expr expr = match expr with
  Syntax.Num(Syntax.Int(i)) -> i
| Syntax.Op(e1,op,e2,v) -> match op with
    Syntax.Plus ->
      let i1 = g_expr e1 in
      let i2 = g_expr e2 in
      let i = i1 + i2 in
      set v i;
      i
    | Syntax.Minus ->
      let i1 = g_expr e1 in
      let i2 = g_expr e2 in
      let i = i1 - i2 in
      set v i;
      i
    | Syntax.Times ->
      let i1 = g_expr e1 in
      let i2 = g_expr e2 in
      let i = i1 * i2 in
      set v i;
      i

exception NotEqual
let g (Evalto(expr,v)) =
  let i = g_expr expr in
  let new_expr = deref_expr expr in
  if VNum(Int(i)) = v then
    Evalto(new_expr,v)
  else (print_string "\nWARNING: the evaluation not equal to the given answer\n";
    Evalto(new_expr,v))

(* Eval.f: start point *)
let f judg = g judg
