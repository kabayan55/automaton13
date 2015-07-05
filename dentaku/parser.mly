%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN
%token RPAREN
%token MINUS
%token TIMES
%token PLUS
%token DIVIDE
%token MOD
%token POWER
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left MOD
%right POWER
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Num ($1) }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
| expr MINUS expr
	{ Syntax.Op2 (Syntax.Minus, $1, $3) }
| expr TIMES expr
	{ Syntax.Op2 (Syntax.Times, $1, $3) }
| expr PLUS expr
	{ Syntax.Op2 (Syntax.Plus, $1, $3) }
| expr DIVIDE expr
	{ Syntax.Op2 (Syntax.Divide, $1, $3) }
| expr MOD expr
	{ Syntax.Op2 (Syntax.Mod, $1, $3) }
| expr POWER expr
	{ Syntax.Op2 (Syntax.Power, $1, $3) }
| MINUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UMinus, $2) }
| PLUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UPlus, $2) }
