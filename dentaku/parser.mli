type token =
  | LPAREN
  | RPAREN
  | MINUS
  | TIMES
  | PLUS
  | DIVIDE
  | MOD
  | POWER
  | NUMBER of (int)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
