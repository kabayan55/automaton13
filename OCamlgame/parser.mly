%{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
open Syntax
%}

/* �ʹߡ��ɤ������櫓�������Ȥ� C ���ˤʤ뤳�Ȥ���� */
/* �ȡ��������� */
%token <string> HOUKOU ITEM TADOUSHI TANDOKUDOUSHI
/* �����ˤ� string �����ͤ�ȼ�����Ȥ򼨤��Ƥ��� */
%token IE HEYA HE NI KARA SUSUMU HAIRU DERU WO DE HASHI WATARU OSHIRO
%token EOL
/* EOL = End Of Line ���Ϥν���� */

/* ��ü����η��򤳤���������� */
%type <Syntax.t> start

/* ���ϵ������� */
%start start

/* ���Ϥ�ʸˡ��

ʸ         = ���� �������� "�ʤ�"
	   | "��" �������� "����"
	   | "��" "����" "�Ф�"
	   | "����" "����" "�Ф�"
	   | "��" "��" "�Ϥ�"
           | ��Ū�� ¾ư��
           | ñ��ư��
����       = "��" | "��" | "��" | "��" | "����" | "����" | "����" | "����"
��������   = "��" | "��"
��Ū��     = �����ƥ� "��" | "��" | "��"
�����ƥ�   = "��" | "�ɥ�" | "���ܥƥ�" | "��" | "������" | "��" | "���ܤꤿ�Ƥε���" | "��" | "����"| "��" | "����" | "�Х�" | "�ڤμ�" | "����" | "���ƻ��"
¾ư��     = "���" | "�֤�" | "�Υå�����" | "����" | "�Ĥ���" | "����" | "��Ǥ�" | "���٤�" | "����" | "��ޤ���"
ñ��ư��   = "��λ����"

���ϡ�Syntax.t ������

*/

/* �ʲ��� %% �Ͼ�ά�Բġ�����ʹߤ�ʸˡ��§��� */
%%

start:
| bun EOL
        { $1 }
| bun anys EOL
        { raise (Error ("��" ^ $2 ^ "�ס�")) }

bun:
| HOUKOU houkoujoshi SUSUMU
        { Idousuru ($1) }
| HOUKOU houkoujoshi
        { raise (Error ("��" ^ $1 ^ "�ˡפɤ����롩")) }
| HOUKOU
        { raise (Error ("��" ^ $1 ^ "�פˤɤ����롩")) }
| IE houkoujoshi HAIRU
        { Idousuru ("��") }
| IE houkoujoshi
        { raise (Error ("�ֲȤˡפɤ����롩")) }
| IE KARA DERU
        { Idousuru ("��") }
| IE KARA
        { raise (Error ("�ֲȤ���פɤ����롩")) }
| IE WO DERU
        { Idousuru ("��") }
| IE WO
        { raise (Error ("�ֲȤ�פɤ����롩")) }
| IE
        { raise (Error ("�ֲȡפˤɤ����롩")) }
| HEYA KARA DERU
        { Idousuru ("��") }
| HEYA KARA
        { raise (Error ("����������פɤ����롩")) }
| HEYA WO DERU
        { Idousuru ("��") }
| HEYA WO
        { raise (Error ("��������פɤ����롩")) }
| HEYA
        { raise (Error ("�������פˤɤ����롩")) }
| HASHI WO WATARU
        { Idousuru ("��") }
| HASHI WO
        { raise (Error ("�ֶ���פɤ����롩")) }
| HASHI
        { raise (Error ("�ֶ��פ�ɤ����롩")) }
| OSHIRO NI HAIRU
        { Idousuru ("����") }
| OSHIRO NI
        { raise (Error ("�֤���ˡפɤ����롩")) }
| OSHIRO
        { raise (Error ("�֤���פ�ϣҤˤɤ����롩")) }
| OSHIRO KARA DERU
        { Idousuru ("�У�") }
| OSHIRO KARA
        { raise (Error ("�֤��뤫��פɤ����롩")) }
| OSHIRO WO DERU
        { Idousuru ("�У�") }
| OSHIRO WO
        { raise (Error ("�֤����פɤ����롩")) }

| mokutekigo TADOUSHI
        { Tadoushi ($1, $2) }
| mokutekigo
        { raise (Error ("��" ^ $1 ^ "��פɤ����롩")) }
| TANDOKUDOUSHI
        { Tandokudoushi ($1) }
| EOL
        { raise (Error ("����")) }

houkoujoshi:
| HE
        { () (* �����֤�ɬ�פ��ʤ� *) }
| NI
        { () (* �����֤�ɬ�פ��ʤ� *) }

mokutekigo:
| ITEM WO
        { $1 }
| ITEM NI
        { $1 }
| ITEM DE
        { $1 }
| ITEM
        { raise (Error ("��" ^ $1 ^ "�פ�ɤ����롩")) }

anys:
any anys        { $1 ^ $2 }

any:
| HOUKOU        { $1 (* �֤�ʸ����ϡ����顼��å������� *) }
| IE            { "��" }
| HEYA          { "����" }
| HASHI         { "��" }
| OSHIRO        { "����" }
| HE            { "��" }
| NI            { "��" }
| KARA          { "����" }
| SUSUMU        { "�ʤ�" }
| HAIRU         { "����" }
| DERU          { "�Ф�" }
| WATARU        { "�Ϥ�" }
| WO            { "��" }
| DE            { "��" }
| ITEM          { $1 }
| TADOUSHI      { $1 }
| TANDOKUDOUSHI { $1 }
