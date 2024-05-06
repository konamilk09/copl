type t =
  | Num of int
  | Bool of bool
  | Fun of nu_t * Syntax.t
  | FunRec of nu_t * Syntax.t
  | Error
  | None

and nu_t = t list

(* 空の値の変数を作る *)
let gen_val () = None

(* nu_t -> int -> Value.t *)
(* 環境と何番目かを受け取って値を返す *)
let rec get nu i =
  match nu with
  | [] -> raise Not_found
  | v :: rest -> if i = 1 then v else get rest (i - 1)

(* 1番最後に入れた要素を返す *)
let last nu = match nu with [] -> raise Not_found | first :: rest -> first

(* 1番最後に入れた要素を取り除いた環境を返す *)
let pop nu = match nu with [] -> [] | first :: rest -> rest

(* 変数と値のペアを受け取って環境に追加する *)
let add nu i = i :: nu

(* print *)
let rec string_of_value v =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Fun (nu, e) ->
      "(" ^ string_of_nu nu ^ ")[fun . -> " ^ Syntax.string_of_expr e ^ "]"
  | FunRec (nu, e) ->
      "(" ^ string_of_nu nu ^ ")[rec . = fun . -> " ^ Syntax.string_of_expr e
      ^ "]"
  | Error -> "error"
  | None -> "none"

and string_of_nu nu =
  match nu with
  | [] -> ""
  | v :: [] -> string_of_value v
  | v :: rest -> string_of_nu rest ^ ", " ^ string_of_value v

exception NotFunV

let get_nu_of_funv v = match v with Fun (nu, e) -> nu | _ -> raise NotFunV
