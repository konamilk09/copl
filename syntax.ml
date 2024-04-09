type op_t = Plus | Times | Minus | Less

type expr_t =
  | Num of int
  | Bool of bool
  | Variable of string * Value.t
  | Op of expr_t * op_t * expr_t * Value.t (* 最後の Value.t は途中の評価結果を保存している *)
  | If of expr_t * expr_t * expr_t * Value.t
  | Let of string * expr_t * expr_t * Value.t

(* Syntax.t: judgement: type of Abstract Syntax Tree which parser generates *)
type t = Evalto of Env.t * expr_t * Value.t

(* 式に保存されている式の評価結果を値として取ってくる *)
let get_value expr =
  match expr with
  | Num n -> Value.Num n
  | Bool b -> Value.Bool b
  | Variable (x, v) -> v
  | Op (_, _, _, v) -> v
  | If (_, _, _, v) -> v
  | Let (_, _, _, v) -> v

let rec string_of_value v =
  match v with
  | Value.Num n -> string_of_int n
  | Value.Bool b -> string_of_bool b
  | Value.Error -> "error"
  | Value.None -> "none"

let rec string_of_env env =
  match env with
  | Env.Emp -> ""
  | Env.Env (Env.Emp, x, v) -> x ^ "=" ^ string_of_value v
  | Env.Env (e, x, v) -> string_of_env e ^ ", " ^ x ^ "=" ^ string_of_value v

let rec string_of_expr_print expr =
  match expr with
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Variable (x, v) -> x
  | Op (e1, op, e2, v) -> (
      match op with
      | Plus ->
          "(" ^ string_of_expr_print e1 ^ "+" ^ string_of_expr_print e2 ^ ")"
      | Times ->
          "(" ^ string_of_expr_print e1 ^ "*" ^ string_of_expr_print e2 ^ ")"
      | Minus ->
          "(" ^ string_of_expr_print e1 ^ "-" ^ string_of_expr_print e2 ^ ")"
      | Less ->
          "(" ^ string_of_expr_print e1 ^ "<" ^ string_of_expr_print e2 ^ ")")
  | If (e1, e2, e3, v) ->
      "if " ^ string_of_expr_print e1 ^ " then " ^ string_of_expr_print e2
      ^ " else " ^ string_of_expr_print e3
  | Let (x, e1, e2, v) ->
      "let " ^ x ^ "=" ^ string_of_expr_print e1 ^ " in "
      ^ string_of_expr_print e2

(* print the AST *)
let rec string_of_judg judg =
  match judg with
  | Evalto (env, expr, v) ->
      string_of_env env ^ " |- " ^ string_of_expr_print expr ^ " evalto "
      ^ string_of_value v

let print judg =
  let str = string_of_judg judg in
  print_string str
