type t =
  | Num of int
  | Bool of bool
  | Fun of env_t * string * Syntax.t
  | FunRec of env_t * string * string * Syntax.t
  | EmptyList
  | List of t * t
  | Error
  | None

and env_t = (string * t) list

type judg_t = Matches of Syntax.pattern_t * t | NMatch of Syntax.pattern_t * t

(* 空の値の変数を作る *)
let gen_val () = None

(* env_t -> string -> Value.t *)
(* 環境と変数を受け取って値を返す *)
let rec get env x =
  match env with
  | [] -> raise Not_found
  | (str, v) :: rest -> if str = x then v else get rest x

(* 1番最後に入れた要素を返す *)
let last env = match env with [] -> raise Not_found | first :: rest -> first

(* 1番最後に入れた要素を取り除いた環境を返す *)
let pop env = match env with [] -> [] | first :: rest -> rest

(* 変数と値のペアを受け取って環境に追加する *)
let add env x v = (x, v) :: env

(* 交わりがない環境を二つ受け取って、結合する *)
(* env2;env1 の順番で結合している *)
(* 交わりがないことは確認していない *)
let union env1 env2 = List.fold_right (fun x acc -> x :: acc) env2 env1

(* env1;env *)
let prepend env env1 = List.fold_right (fun x acc -> x :: acc) env env1

(* env;env1 *)
let append env env1 = List.fold_right (fun x acc -> x :: acc) env1 env

(* print *)
let rec string_of_value v =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Fun (env, x, e) ->
      "(" ^ string_of_env env ^ ")[fun " ^ x ^ " -> " ^ Syntax.string_of_expr e
      ^ "]"
  | FunRec (env, x, y, e) ->
      "(" ^ string_of_env env ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> "
      ^ Syntax.string_of_expr e ^ "]"
  | EmptyList -> "[]"
  | List (f, r) -> "(" ^ string_of_value f ^ "::" ^ string_of_value r ^ ")"
  | Error -> "error"
  | None -> "none"

and string_of_env env =
  match env with
  | [] -> ""
  | (x, v) :: [] -> x ^ "=" ^ string_of_value v
  | (x, v) :: rest -> string_of_env rest ^ ", " ^ x ^ "=" ^ string_of_value v

let string_of_judg judg =
  match judg with
  | Matches (p, v) ->
      Syntax.string_of_pattern p ^ " matches " ^ string_of_value v
  | NMatch (p, v) ->
      Syntax.string_of_pattern p ^ " doesn't match " ^ string_of_value v

exception NotFunV

let get_env_of_funv v =
  match v with Fun (env, x, e) -> env | _ -> raise NotFunV
