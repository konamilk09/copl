(* メイン関数 *)
let go () =
  let judg = Parser.start Lexer.token (Lexing.from_channel stdin) in
  (* これで標準入力を字句解析して、構文解析した結果を judg に入れ *)
  print_string "Parsed : ";
  Syntax.print judg;    (* 入力を表示する *)
  print_newline ();
  (* 計算結果を表示する *)
  let judg = Eval.f judg in
  print_string "Evaluated : ";
  Eval.print_eval judg;    (* 評価付きを表示する *)
  print_newline ();
  print_string "Tree :\n";
  let str = Tree.f judg in
  print_string str

(* スタートアップ *)
let _ = go ()