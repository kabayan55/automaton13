%{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
%}

/* �ʹߡ��ɤ������櫓�������Ȥ� C ���ˤʤ뤳�Ȥ���� */
/* �ȡ��������� */
%token LPAREN
%token RPAREN
%token MINUS
%token TIMES
%token PLUS
%token DIVIDE
%token MOD
%token POWER
%token <int> NUMBER
/* ����ϡ������ˤ� int �����ͤ�ȼ�����Ȥ򼨤��Ƥ��� */
%token EOF
/* End of File: ���Ϥν����򼨤� */

/* ��ü����η��򤳤���������� */
%type <Syntax.t> expr

/* ���ϵ������� */
%start expr

/* �黻�Ҥ�ͥ���̤���ꤹ�� */
/* ���˹Ԥ��ۤɶ�����礹�� */
%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left MOD
%right POWER
%nonassoc UNARY
/* nonassoc �Ϸ��ʤ�����󡢤��ä���񤫤ʤ��ƤϤʤ�ʤ��ˡ�
   left �Ϻ���硢right �ϱ���� */

/* �ʲ��� %% �Ͼ�ά�Բġ�����ʹߤ�ʸˡ��§��� */
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
