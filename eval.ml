open Syntax

(* デバッグ用 print *)
let rec string_of_expr_eval expr =
  match expr with
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Variable (x, v) -> x ^ "[" ^ string_of_value v ^ "]"
  | Op (e1, op, e2, v) -> (
      match op with
      | Plus ->
          "(" ^ string_of_expr_eval e1 ^ "+" ^ string_of_expr_eval e2 ^ "["
          ^ string_of_value v ^ "]" ^ ")"
      | Times ->
          "(" ^ string_of_expr_eval e1 ^ "*" ^ string_of_expr_eval e2 ^ "["
          ^ string_of_value v ^ "]" ^ ")"
      | Minus ->
          "(" ^ string_of_expr_eval e1 ^ "-" ^ string_of_expr_eval e2 ^ "["
          ^ string_of_value v ^ "]" ^ ")"
      | Less ->
          "(" ^ string_of_expr_eval e1 ^ "<" ^ string_of_expr_eval e2 ^ "["
          ^ string_of_value v ^ "]" ^ ")")
  | If (e1, e2, e3, v) ->
      "(if " ^ string_of_expr_eval e1 ^ " then " ^ string_of_expr_eval e2
      ^ " else " ^ string_of_expr_eval e3 ^ "[" ^ string_of_value v ^ "])"
  | Let (x, e1, e2, v) ->
      "(let " ^ x ^ "=" ^ string_of_expr_print e1 ^ " in "
      ^ string_of_expr_print e2 ^ "[" ^ string_of_value v ^ "])"

(* print the AST *)
let rec string_of_judg_eval judg =
  match judg with
  | Evalto (env, expr, v) ->
      string_of_env env ^ " |- " ^ string_of_expr_eval expr ^ " evalto "
      ^ string_of_value v

let print_eval judg =
  let str = string_of_judg_eval judg in
  print_string str

exception NotSupported of string

let eval_op v1 v2 op =
  match (v1, v2) with
  | Value.Num i1, Value.Num i2 -> (
      match op with
      | Plus -> Value.Num (i1 + i2)
      | Minus -> Value.Num (i1 - i2)
      | Times -> Value.Num (i1 * i2)
      | Less -> Value.Bool (i1 < i2))
  | Value.Bool i1, _ -> Value.Error
  | _, Value.Bool i2 -> Value.Error
  | Value.Error, _ -> Value.Error
  | _, Value.Error -> Value.Error
  | _ -> raise (NotSupported "None")

let eval_if v1 =
  match v1 with
  | Value.Bool b -> Value.Bool b
  | Value.Num i -> Value.Error
  | Value.Error -> Value.Error
  | _ -> raise (NotSupported "None")

let rec g_expr expr env =
  match expr with
  | Num n -> Num n
  | Bool b -> Bool b
  | Variable (x, v) -> Variable (x, Env.get env x)
  | Op (e1, op, e2, v) ->
      let t1 = g_expr e1 env in
      let t2 = g_expr e2 env in
      let v1 = get_value t1 in
      let v2 = get_value t2 in
      let new_v = eval_op v1 v2 op in
      Op (t1, op, t2, new_v)
  | If (e1, e2, e3, v) -> (
      let t1 = g_expr e1 env in
      let v1 = get_value t1 in
      let i = eval_if v1 in
      match i with
      | Value.Bool true ->
          let t2 = g_expr e2 env in
          let v2 = get_value t2 in
          If (t1, t2, e3, v2)
      | Value.Bool false ->
          let t3 = g_expr e3 env in
          let v3 = get_value t3 in
          If (t1, e2, t3, v3)
      | Value.Error -> If (t1, e2, e3, Value.Error)
      | _ -> If (t1, e2, e3, Value.Error)
      (* eval_if で Value.Bool と Value.Error 以外返してないからここには来ない *))
  | Let (x, e1, e2, v) ->
      let t1 = g_expr e1 env in
      let v1 = get_value t1 in
      let new_env = Env.add env x v1 in
      let t2 = g_expr e2 new_env in
      let v2 = get_value t2 in
      Let (x, t1, t2, v2)

exception NotEqual

let g (Evalto (env, expr, v)) =
  let new_expr = g_expr expr env in
  let res = get_value new_expr in
  if res = v then Evalto (env, new_expr, v)
  else (
    print_string "WARNING: the evaluation not equal to the given answer\n";
    Evalto (env, new_expr, res))

(* Eval.f: start point *)
let f judg = g judg
