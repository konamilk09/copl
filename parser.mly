%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* トークンの定義 */    /* 以降、コメントが C 式になることに注意 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES LESS
%token EVALTO IF THEN ELSE
%token <int> NUMBER     /* これは、整数には int 型の値が伴うことを示す */
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
%nonassoc UNARY
/* nonassoc は結合なし（毎回、括弧が必要）、left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| judgement EOF               { $1 }

simple_expr:
| NUMBER                 { Syntax.Num ($1) }
| TRUE                   { Syntax.Bool ($1) }
| FALSE                  { Syntax.Bool ($1) }
| LPAREN expr RPAREN     { $2 }

expr:
| simple_expr            { $1 }
| expr PLUS expr         { Syntax.Op ($1, Syntax.Plus, $3, Syntax.gen_val()) }
| expr MINUS expr        { Syntax.Op ($1, Syntax.Minus, $3, Syntax.gen_val()) }
| expr TIMES expr        { Syntax.Op ($1, Syntax.Times, $3, Syntax.gen_val()) }
| expr LESS expr         { Syntax.Op ($1, Syntax.Less, $3, Syntax.gen_val()) }
| IF expr THEN expr ELSE expr        { Syntax.If ($2, $4, $6, Syntax.gen_val()) }
// | MINUS expr %prec UNARY { Syntax.Op (Syntax.Num ( Syntax.Int(0)), Syntax.Minus, $2) }

value:
| NUMBER                 { Syntax.VNum ($1) }
| TRUE                   { Syntax.VBool ($1) }
| FALSE                  { Syntax.VBool ($1) }

judgement:
| expr EVALTO value {Syntax.Evalto ($1, $3)}
