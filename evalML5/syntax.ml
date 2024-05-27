type op_t = Plus | Times | Minus | Lt

type pattern_t =
  | PatternV of string
  | PatternEmpty
  | Cons of pattern_t * pattern_t
  | WildCard

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
  | EmptyList
  | List of t * t
  | Match of t * clause_t

and clause_t = Clause1 of pattern_t * t | Clause2 of pattern_t * t * clause_t

let string_of_op op =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

let rec string_of_pattern p =
  match p with
  | PatternV x -> x
  | PatternEmpty -> "[]"
  | Cons (p1, p2) ->
      "(" ^ string_of_pattern p1 ^ "::" ^ string_of_pattern p2 ^ ")"
  | WildCard -> "_"

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
  | EmptyList -> "[]"
  | List (e1, e2) -> "(" ^ string_of_expr e1 ^ "::" ^ string_of_expr e2 ^ ")"
  | Match (e1, c) ->
      "match " ^ string_of_expr e1 ^ " with " ^ string_of_clauses c

and string_of_clauses c =
  match c with
  | Clause1 (p, e) -> string_of_pattern p ^ " -> " ^ string_of_expr e
  | Clause2 (p, e, c) ->
      string_of_pattern p ^ " -> " ^ string_of_expr e ^ " | "
      ^ string_of_clauses c

let print expr =
  let str = string_of_expr expr in
  print_string str
