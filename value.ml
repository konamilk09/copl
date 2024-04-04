type t = Num of int
       | Bool of bool
       | Error
       | None

(* 空の値の変数を作る *)
let gen_val () = None
