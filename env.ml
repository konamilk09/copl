type t = Emp | Env of t * string * Value.t (* Env, x = v *)

(* Env の操作 *)
(* string -> env_t -> value_t *)
(* 環境の右からひとつずつ見て行き、stringと一致した変数の値を返す *)
let rec get env x =
  match env with
  | Emp ->
      print_string "ERROR!Not found variable in the environment";
      Value.None
  | Env (new_env, y, v) -> if y = x then v else get new_env x

(* 環境の右にキーとバリューのペアを追加する *)
let rec add env key value = Env (env, key, value)

(* 環境の右端のペアを1つ取り除く *)
let rec pop env =
  match env with
  | Emp ->
      print_string "WARNING: Attempt to remove a pair from an empty env";
      Emp
  | Env (new_env, x, v) -> new_env
