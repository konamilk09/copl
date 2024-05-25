(* メイン関数 *)
let go () =
  (* 標準入力を字句解析して、構文解析した結果を expr に入れる *)
  let expr = Parser.start Lexer.token (Lexing.from_channel stdin) in

  (* 入力を表示する *)
  (* print_string "Parsed : "; *)
  (* Syntax.print expr; *)
  (* 導出木を表示する *)
  let eexpr = Eval.f expr in
  (* print_string "Evaluated :\n"; *)
  Eval.print eexpr;
  print_newline ()

(* スタートアップ *)
let _ = go ()