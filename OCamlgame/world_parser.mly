%{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
open World_syntax
%}

/* �ʹߡ��ɤ������櫓�������Ȥ� C ���ˤʤ뤳�Ȥ���� */
/* �ȡ��������� */
%token <string> SECTION SUBSECTION TEXT INDENTED_TEXT
/* �����ˤ� string �����ͤ�ȼ�����Ȥ򼨤��Ƥ��� */
%token SHOKIMESSAGE SHURYOUJOUKEN
%token EOF
/* EOF = End Of File ���Ϥν���� */

/* ��ü����η��򤳤���������� */
%type <World_syntax.t> start

/* ���ϵ������� */
%start start

/* ����ե����� (world.txt) �ι�¤��

����ե����� = ����� ����������� ��λ��

�����       = "===�����å�����==="
               �ƥ�������

���������   = "===���������̾==="
               �ڥ�
               �������������

����������� = "---�����������̾---"
               �ڥ���

��λ��       = "===��λ���==="
               �ڥ���

�ڥ�         = �ƥ�����
               �����եƥ�������

�ѡ����ν��ϡ�World_syntax.t ������

*/

/* �ʲ��� %% �Ͼ�ά�Բġ�����ʹߤ�ʸˡ��§��� */
%%

start:
| shoki_section sections shuryou_section EOF
        { { messages = $1;
            sections = $2;
            shuryo_jouken = $3; } }

shoki_section:
| SHOKIMESSAGE messages
        { $2 }

messages:
|
        { [] }
| TEXT messages
        { $1 :: $2 }

sections:
|
        { [] }
| section sections
        { $1 :: $2 }

section:
| SECTION pair subsections
        { ($1, { initial_pair = $2; subsections = $3 }) }

subsections:
|
        { [] }
| subsection subsections
        { $1 :: $2 }

subsection:
| SUBSECTION pairs
        { ($1, $2) }

pair:
| TEXT indented_texts
        { ($1, $2) }

pairs:
|
        { [] }
| pair pairs
        { $1 :: $2 }

indented_texts:
|
        { [] }
| INDENTED_TEXT indented_texts
        { $1 :: $2 }

shuryou_section:
| SHURYOUJOUKEN pairs
        { $2 }
