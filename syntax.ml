type value_t = VNum of int
             | VBool of bool
             | VError
             | VVar of (value_t option ref) (* 途中の評価結果を保存する変数 *)
type op_t = Plus | Times | Minus | Less
type expr_t = Num of int
            | Bool of bool
            | Op of expr_t * op_t * expr_t * value_t (* 最後の value_t は途中の評価結果を保存している *)
            | If of expr_t * expr_t * expr_t * value_t

(* 空の値の変数を作る *)
let gen_val () = VVar (ref None)

(* Syntax.t: judgement: type of Abstract Syntax Tree which parser generates *)
type t = Evalto of expr_t * value_t

let rec string_of_value v = match v with
  VNum (n) -> string_of_int n
| VBool (b) -> string_of_bool b
| VError -> "error"
| VVar (v) -> match !v with
    None -> "none"
  | Some(some) -> string_of_value some

let rec string_of_expr_print expr = match expr with
  Num (i) -> string_of_int i
| Bool (b) -> string_of_bool b
| Op (e1,op,e2,v) -> begin match op with
    Plus -> "("
      ^ string_of_expr_print e1
      ^ "+"
      ^ string_of_expr_print e2 ^ ")"
  | Times -> "("
      ^ string_of_expr_print e1
      ^ "*"
      ^ string_of_expr_print e2 ^ ")"
  | Minus -> "("
      ^ string_of_expr_print e1
      ^ "-"
      ^ string_of_expr_print e2 ^ ")"
  | Less -> "("
      ^ string_of_expr_print e1
      ^ "<"
      ^ string_of_expr_print e2 ^ ")"
  end
| If (e1,e2,e3,v) -> "if "
    ^ string_of_expr_print e1
    ^ " then "
    ^ string_of_expr_print e2
    ^ " else "
    ^ string_of_expr_print e3
  (* print the AST *)
let rec string_of_judg_print judg = match judg with
  Evalto (e,v) ->
    string_of_expr_print e
    ^ " evalto "
    ^ string_of_value v

let print judg =
  let str = string_of_judg_print judg
in print_string str
