type op_t = Plus | Times | Minus | Lt

type t =
  | Num of int
  | Bool of bool
  | Variable of string
  | Op of t * op_t * t
  | If of t * t * t
  | Let of string * t * t
  | LetRec of string * string * t * t
  | Fun of string * t
  | App of t * t

let string_of_op op =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

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
  | LetRec (x, y, e1, e2) ->
      "let rec " ^ x ^ "=" ^ "fun " ^ y ^ " -> " ^ string_of_expr e1 ^ " in "
      ^ string_of_expr e2
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ string_of_expr e ^ ")"
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"

(* print the AST *)
(* let rec string_of_judg judg =
   match judg with
   | Evalto (env, expr, v) ->
       string_of_env env ^ " |- " ^ string_of_expr expr ^ " evalto "
       ^ string_of_value v *)

let print expr =
  let str = string_of_expr expr in
  print_string str
