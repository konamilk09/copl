%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* トークンの定義 */    /* 以降、コメントが C 式になることに注意 */
%token LPAREN RPAREN
%token <string> VARIABLE
%token EQUAL COMMA
%token PLUS MINUS TIMES LESS
%token UNDER
%token EVALTO ERROR
%token IF THEN ELSE
%token LET IN
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
| VARIABLE               { Syntax.Variable ($1, Value.gen_val()) }

expr:
| simple_expr            { $1 }
| expr PLUS expr         { Syntax.Op ($1, Syntax.Plus, $3, Value.gen_val()) }
| expr MINUS expr        { Syntax.Op ($1, Syntax.Minus, $3, Value.gen_val()) }
| expr TIMES expr        { Syntax.Op ($1, Syntax.Times, $3, Value.gen_val()) }
| expr LESS expr         { Syntax.Op ($1, Syntax.Less, $3, Value.gen_val()) }
| IF expr THEN expr ELSE expr        { Syntax.If ($2, $4, $6, Value.gen_val()) }
| LET VARIABLE EQUAL expr IN expr    { Syntax.Let ($2, $4, $6, Value.gen_val()) }

value:
| NUMBER                 { Value.Num ($1) }
| TRUE                   { Value.Bool ($1) }
| FALSE                  { Value.Bool ($1) }
| ERROR                  { Value.Error }

env:
| env COMMA VARIABLE EQUAL value { Env.Env($1, $3, $5) }
| VARIABLE EQUAL value { Env.Env(Env.Empty, $1, $3) }
// | { Env.Empty }

judgement:
| env UNDER expr EVALTO value { Syntax.Evalto ($1, $3, $5) }
| UNDER expr EVALTO value { Syntax.Evalto (Env.Empty, $2, $4) }
