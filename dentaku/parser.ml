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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* 補助的な変数、関数、型などの定義 *)
# 18 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* MINUS *);
  260 (* TIMES *);
  261 (* PLUS *);
  262 (* DIVIDE *);
  263 (* MOD *);
  264 (* POWER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  265 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\003\000\
\000\000\010\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000"

let yysindex = "\002\000\
\255\254\000\000\255\254\255\254\255\254\000\000\038\255\000\000\
\032\255\000\000\000\000\255\254\255\254\255\254\255\254\255\254\
\255\254\000\000\019\255\041\255\008\255\254\254\001\255\001\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\018\000\024\000\013\000\001\000\007\000"

let yygindex = "\000\000\
\016\000\000\000"

let yytablesize = 285
let yytable = "\003\000\
\008\000\004\000\001\000\005\000\016\000\017\000\009\000\006\000\
\017\000\012\000\012\000\013\000\007\000\015\000\016\000\017\000\
\000\000\005\000\009\000\010\000\011\000\004\000\013\000\006\000\
\015\000\016\000\017\000\019\000\020\000\021\000\022\000\023\000\
\024\000\018\000\012\000\013\000\014\000\015\000\016\000\017\000\
\012\000\013\000\014\000\015\000\016\000\017\000\015\000\016\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\007\000\007\000\
\007\000\007\000\007\000\005\000\005\000\005\000\005\000\004\000\
\004\000\006\000\004\000\000\000\006\000"

let yycheck = "\001\001\
\000\000\003\001\001\000\005\001\007\001\008\001\000\000\009\001\
\008\001\000\000\003\001\004\001\000\000\006\001\007\001\008\001\
\255\255\000\000\003\000\004\000\005\000\000\000\004\001\000\000\
\006\001\007\001\008\001\012\000\013\000\014\000\015\000\016\000\
\017\000\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\003\001\004\001\005\001\006\001\007\001\008\001\006\001\007\001\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\002\001\003\001\004\001\005\001\006\001\007\001\002\001\003\001\
\004\001\005\001\006\001\002\001\003\001\004\001\005\001\002\001\
\003\001\002\001\005\001\255\255\005\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  MINUS\000\
  TIMES\000\
  PLUS\000\
  DIVIDE\000\
  MOD\000\
  POWER\000\
  EOF\000\
  "

let yynames_block = "\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
 ( Syntax.Num (_1) )
# 164 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.t) in
    Obj.repr(
# 45 "parser.mly"
 ( _2 )
# 171 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 49 "parser.mly"
 ( _1 )
# 178 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 51 "parser.mly"
 ( Syntax.Op2 (Syntax.Minus, _1, _3) )
# 186 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 53 "parser.mly"
 ( Syntax.Op2 (Syntax.Times, _1, _3) )
# 194 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 55 "parser.mly"
 ( Syntax.Op2 (Syntax.Plus, _1, _3) )
# 202 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 57 "parser.mly"
 ( Syntax.Op2 (Syntax.Divide, _1, _3) )
# 210 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 59 "parser.mly"
 ( Syntax.Op2 (Syntax.Mod, _1, _3) )
# 218 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 61 "parser.mly"
 ( Syntax.Op2 (Syntax.Power, _1, _3) )
# 226 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 63 "parser.mly"
 ( Syntax.Op1 (Syntax.UMinus, _2) )
# 233 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 65 "parser.mly"
 ( Syntax.Op1 (Syntax.UPlus, _2) )
# 240 "parser.ml"
               : Syntax.t))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
