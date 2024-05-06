%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax
%}

/* トークンの定義 */    /* 以降、コメントが C 式になることに注意 */
%token LPAREN RPAREN
%token <string> VARIABLE
%token EQUAL COMMA DOT
%token PLUS MINUS TIMES LESS
%token UNDER ARROW
%token EVALTO ERROR
%token IF THEN ELSE
%token LET REC IN
%token FUN
%token <int> NUMBER     /* これは、整数には int 型の値が伴うことを示す */
%token <int> SHARP
%token <bool> TRUE
%token <bool> FALSE
%token EOF              /* End of File: 入力の終わりを示す */

/* エントリーポイント（開始記号）の定義 */
%start start

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%left LESS
%left PLUS MINUS
%left TIMES
%left prec_app
%nonassoc UNARY
/* nonassoc は結合なし（毎回、括弧が必要）、left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| expr EOF               { $1 }

simple_expr:
| NUMBER                 { Num ($1) }
| TRUE                   { Bool ($1) }
| FALSE                  { Bool ($1) }
| LPAREN expr RPAREN     { $2 }
| SHARP                  { Sharp ($1) }

expr:
| simple_expr            { $1 }
| expr PLUS expr         { Op ($1, Plus, $3) }
| expr MINUS expr        { Op ($1, Minus, $3) }
| expr TIMES expr        { Op ($1, Times, $3) }
| expr LESS expr         { Op ($1, Lt, $3) }
| IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
| LET DOT EQUAL expr IN expr    { Let ($4, $6) }
| LET REC DOT EQUAL FUN DOT ARROW expr IN expr    { LetRec ($8, $10) }
| FUN DOT ARROW expr            { Fun ($4) }
| expr simple_expr %prec prec_app           { App ($1, $2) }
// expr simple_expr じゃないと一つの引数で左結合にならない。
// max 3 5 -> max (3 5)
