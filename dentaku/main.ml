(* �ᥤ��ؿ� *)
let go () =
  let expr = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  (* �����ɸ�����Ϥ������Ϥ��ơ���ʸ���Ϥ�����̤� expr ������ *)
  print_string "Parsed : ";
  Syntax.print expr;		(* ���Ϥ�ɽ������ *)
  print_newline ();
  print_string "Result : ";
  print_int (Eval.f expr);
  print_newline ()

(* �������ȥ��å� *)
let _ = go ()
