open Syntax

(* デバッグ用 print *)
let rec string_of_expr_eval expr = match expr with
  Num (i) -> string_of_int i
| Bool (b) -> string_of_bool b
| Op (e1,op,e2,v) -> begin match op with
    Plus -> "("
      ^ string_of_expr_eval e1
      ^ "+"
      ^ string_of_expr_eval e2
      ^ "[" ^ string_of_value v ^ "]"
      ^ ")"
  | Times -> "("
      ^ string_of_expr_eval e1
      ^ "*"
      ^ string_of_expr_eval e2
      ^ "[" ^ string_of_value v ^ "]"
      ^ ")"
  | Minus -> "("
      ^ string_of_expr_eval e1
      ^ "-"
      ^ string_of_expr_eval e2
      ^ "[" ^ string_of_value v ^ "]"
      ^ ")"
  | Less -> "("
      ^ string_of_expr_eval e1
      ^ "<"
      ^ string_of_expr_eval e2
      ^ "[" ^ string_of_value v ^ "]"
      ^ ")"
end
| If(e1,e2,e3,v) -> "(if "
    ^ string_of_expr_eval e1
    ^ " then "
    ^ string_of_expr_eval e2
    ^ " else "
    ^ string_of_expr_eval e3
    ^ "[" ^ string_of_value v ^ "])"

(* print the AST *)
let rec string_of_judg_eval judg = match judg with
  Evalto (e,v) -> string_of_expr_eval e
    ^ " evalto "
    ^ string_of_value v

let print_eval judg =
  let str = string_of_judg_eval judg
in print_string str

exception NotSupported of string
let eval_op v1 v2 op = begin match (v1,v2) with
  | (VNum(i1),VNum(i2)) -> begin match op with
    | Plus -> VNum(i1+i2)
    | Minus -> VNum(i1-i2)
    | Times -> VNum(i1*i2)
    | Less -> VBool(i1<i2)
    end
  | (VBool(i1),_) -> VError
  | (_,VBool(i2)) -> VError
  | (VError, _) -> VError
  | (_, VError) -> VError
  | _ -> raise (NotSupported "VNone")
  end
let eval_if v1 = match v1 with
| VBool(b) -> VBool(b)
| VNum(i) -> VError
| VError -> VError
| _ -> raise (NotSupported "VNone")

let rec g_expr expr = match expr with
  Num(n) -> Num(n)
| Bool(b) -> Bool(b)
| Op(e1,op,e2,v) ->
    let t1 = g_expr e1 in
    let t2 = g_expr e2 in
    let v1 = get_value t1 in
    let v2 = get_value t2 in
    let new_v = eval_op v1 v2 op in
    Op(t1,op,t2,new_v)
| If(e1,e2,e3,v) ->
    let t1 = g_expr e1 in
    let v1 = get_value t1 in
    let i = eval_if v1 in
    begin match i with
    | VBool(true) ->
        let t2 = g_expr e2 in
        let v2 = get_value t2 in
        If(t1,t2,e3,v2)
    | VBool(false) ->
        let t3 = g_expr e3 in
        let v3 = get_value t3 in
        If(t1,e2,t3,v3)
    | VError ->
        If(t1,e2,e3,VError)
    | _ -> If(t1,e2,e3,VError) (* eval_if で VBool と VError 以外返してないからここには来ない *)
    end

exception NotEqual
let g (Evalto(expr,v)) =
  let new_expr = g_expr expr in
  let res = get_value new_expr in
  if res = v then
    Evalto(new_expr,v)
  else (print_string "\nWARNING: the evaluation not equal to the given answer\n";
    Evalto(new_expr,res))

(* Eval.f: start point *)
let f judg = g judg
