(* ������ϡ���ʸ������Υ��顼 *)
exception Error of string

(* Syntax.t : �桼��������ʸ�η� *)
type t = Idousuru of string		(* ��ư���� *)
       | Tadoushi of string * string	(* ¾ư�� *)
       | Tandokudoushi of string	(* ñ��ư�� *)
