open Syntax

(* デバッグ用 print *)
let rec string_of_expr_eval expr = match expr with
  Num (i) -> string_of_int i
| Bool (b) -> string_of_bool b
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
  VNum(n) -> ()
| VBool(b) -> ()
| VError -> ()
| VVar(v) -> v := Some(i)

(* deref のときにまだ評価されていない変数があったらエラー *)
exception UnEval
let rec deref_value v = match v with
  VNum (n) -> v
| VBool (b) -> v
| VError -> v
| VVar ({contents=None}) -> raise UnEval
| VVar ({contents=Some(VNum(n))}) -> VNum(n)
| VVar ({contents=Some(VBool(b))}) -> VBool(b)
| VVar ({contents=Some(VError)}) -> VError
| VVar ({contents=Some(VVar(_) as vv)}) -> deref_value vv
let rec deref_expr expr = match expr with
  Num (n) -> expr
| Bool (b) -> expr
| Op (e1,op,e2,v) ->
    Op(deref_expr e1, op, deref_expr e2, deref_value v)

exception NotSupported of string
let eval_op v1 v2 op = match (v1,v2) with
  (VNum(i1),VNum(i2)) -> VNum(op i1 i2)
(* | (VBool(i1),VNum(i2)) -> VError
| (VNum(i1),VBool(i2)) -> VError
| (VBool(i1),VBool(i2)) -> VError *)
| _ -> raise (NotSupported "calculation of not integer values are not supported in EvalML1")

let rec g_expr expr = match expr with
  Syntax.Num(n) -> VNum(n)
| Syntax.Bool(b) -> VBool(b)
| Syntax.Op(e1,op,e2,v) -> match op with
    Syntax.Plus ->
      let v1 = g_expr e1 in
      let v2 = g_expr e2 in
      let i = (eval_op v1 v2 (+)) in
      set v i;
      i
    | Syntax.Minus ->
      let v1 = g_expr e1 in
      let v2 = g_expr e2 in
      let i = (eval_op v1 v2 (-)) in
      set v i;
      i
    | Syntax.Times ->
      let v1 = g_expr e1 in
      let v2 = g_expr e2 in
      let i = (eval_op v1 v2 ( * )) in
      set v i;
      i

exception NotEqual
let g (Evalto(expr,v)) =
  let i = g_expr expr in
  let new_expr = deref_expr expr in
  if i = v then
    Evalto(new_expr,v)
  else (print_string "\nWARNING: the evaluation not equal to the given answer\n";
    Evalto(new_expr,v))

(* Eval.f: start point *)
let f judg = g judg
