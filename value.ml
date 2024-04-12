type t =
  | Num of int
  | Bool of bool
  | Fun of env_t * string * Syntax.t
  | Error
  | None

and env_t = (string * t) list

(* 空の値の変数を作る *)
let gen_val () = None
