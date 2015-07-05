{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
open Parser
}

(* ����ɽ����ά�� *)
(* [...] ����� character '...' �Ǥʤ��ƤϤʤ�ʤ� *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* ���ڡ������ɤ����Ф� *)
| "(*" [^ '\n']* "\n"           (* ( * ��������ޤǤϥ����� *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "-"	 { MINUS }
| "*"	 { TIMES }
| "+"	 { PLUS }
| "/"	 { DIVIDE }
| "mod"	 { MOD }
| "^"	 { POWER }
| digit+                        (* ���������İʾ� *)
	 { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| eof	 { EOF }                (* ���Ͻ�λ *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
