type op_t = Plus | Times | Minus | Less

type t =
  | Num of int
  | Bool of bool
  | Variable of string
  | Op of t * op_t * t
  | If of t * t * t
  | Let of string * t * t
  | Fun of string * t
  | App of t * t

(* 式に保存されている式の評価結果を値として取ってくる *)
(* let get_value expr =
   match expr with
   | Num n -> Value.Num n
   | Bool b -> Value.Bool b
   | Variable (x, v) -> v
   | Op (_, _, _, v) -> v
   | If (_, _, _, v) -> v
   | Let (_, _, _, v) -> v
   | Fun (_, _, v) -> v
   | App (_, _, v) -> v *)

(* let rec string_of_value v =
   match v with
   | Value.Num n -> string_of_int n
   | Value.Bool b -> string_of_bool b
   | Value.Error -> "error"
   | Value.None -> "none" *)

(* let rec string_of_env env =
   match env with
   | Env.Emp -> ""
   | Env.Env (Env.Emp, x, v) -> x ^ "=" ^ string_of_value v
   | Env.Env (e, x, v) -> string_of_env e ^ ", " ^ x ^ "=" ^ string_of_value v *)

let string_of_op op =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Less -> "<"

let rec string_of_expr expr =
  match expr with
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Variable x -> x
  | Op (e1, op, e2) ->
      "(" ^ string_of_expr e1 ^ string_of_op op ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else "
      ^ string_of_expr e3
  | Let (x, e1, e2) ->
      "let " ^ x ^ "=" ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ string_of_expr e
  | App (e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2

(* print the AST *)
(* let rec string_of_judg judg =
   match judg with
   | Evalto (env, expr, v) ->
       string_of_env env ^ " |- " ^ string_of_expr expr ^ " evalto "
       ^ string_of_value v *)

let print expr =
  let str = string_of_expr expr in
  print_string str
