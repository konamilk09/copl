{
(* 補助的な変数、関数、型などの定義 *)
open Parser
open Str
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

(* 字句解析の規則 *)
rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
         { token lexbuf }
| "("    { LPAREN }
| ")"    { RPAREN }
| "+"    { PLUS }
| "-"    { MINUS }
| "*"    { TIMES }
| "<"    { LESS }
| "="    { EQUAL }
| ","    { COMMA }
| "."    { DOT }
| "|-"   { UNDER }
| "->"   { ARROW }
| "evalto"  { EVALTO }
| "error"   { ERROR }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "let"     { LET }
| "rec"     { REC }
| "in"      { IN }
| "fun"      { FUN }
| "true"    { TRUE (bool_of_string (Lexing.lexeme lexbuf)) }
| "false"   { FALSE (bool_of_string (Lexing.lexeme lexbuf)) }
| digit+                        (* 数字が１個以上 *)
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| "-" digit+                    (* 負の数 *)
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| "#" digit+                    (* #から始まる数 *)
         { SHARP (int_of_string (Str.string_after (Lexing.lexeme lexbuf) 1)) }
| lower+ (alpha|digit)* { VARIABLE (Lexing.lexeme lexbuf) }
| eof    { EOF }                (* 入力終了 *)
| _      { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }