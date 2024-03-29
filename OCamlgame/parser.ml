type token =
  | HOUKOU of (string)
  | ITEM of (string)
  | TADOUSHI of (string)
  | TANDOKUDOUSHI of (string)
  | IE
  | HEYA
  | HE
  | NI
  | KARA
  | SUSUMU
  | HAIRU
  | DERU
  | WO
  | DE
  | HASHI
  | WATARU
  | OSHIRO
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* 補助的な変数、関数、型などの定義 *)
open Syntax
# 27 "parser.ml"
let yytransl_const = [|
  261 (* IE *);
  262 (* HEYA *);
  263 (* HE *);
  264 (* NI *);
  265 (* KARA *);
  266 (* SUSUMU *);
  267 (* HAIRU *);
  268 (* DERU *);
  269 (* WO *);
  270 (* DE *);
  271 (* HASHI *);
  272 (* WATARU *);
  273 (* OSHIRO *);
  274 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* HOUKOU *);
  258 (* ITEM *);
  259 (* TADOUSHI *);
  260 (* TANDOKUDOUSHI *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\004\000\
\004\000\005\000\005\000\005\000\005\000\003\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\002\000\001\000\003\000\002\000\003\000\
\002\000\003\000\002\000\001\000\003\000\002\000\003\000\002\000\
\001\000\003\000\002\000\001\000\003\000\002\000\001\000\003\000\
\002\000\003\000\002\000\002\000\001\000\001\000\001\000\001\000\
\001\000\002\000\002\000\002\000\001\000\002\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\031\000\056\000\000\000\000\000\032\000\033\000\000\000\
\035\000\034\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\053\000\054\000\055\000\
\040\000\041\000\044\000\045\000\046\000\047\000\048\000\049\000\
\051\000\052\000\042\000\050\000\043\000\001\000\000\000\000\000\
\028\000\003\000\008\000\010\000\006\000\013\000\015\000\018\000\
\021\000\024\000\026\000\002\000\038\000"

let yydgoto = "\002\000\
\011\000\012\000\047\000\016\000\013\000\048\000"

let yysindex = "\029\000\
\067\000\000\000\019\000\066\000\000\000\068\000\096\000\025\255\
\095\000\000\000\000\000\255\254\060\255\000\000\000\000\073\255\
\000\000\000\000\000\000\106\255\165\255\090\255\178\255\228\255\
\121\255\144\255\229\255\248\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\191\255\085\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\233\255\013\000\000\000\049\000\251\255\017\255\
\031\000\000\000\000\000\000\000\035\255\000\000\000\000\053\255\
\000\000\000\000\000\000\071\255\089\255\107\255\125\255\143\255\
\161\255\179\255\197\255\215\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\179\000\002\001\000\000\000\000"

let yytablesize = 365
let yytable = "\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\001\000\020\000\020\000\
\020\000\020\000\020\000\029\000\029\000\025\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\049\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\050\000\009\000\009\000\009\000\009\000\009\000\
\009\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\053\000\011\000\011\000\011\000\
\011\000\011\000\011\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\051\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\056\000\014\000\014\000\014\000\014\000\014\000\014\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\057\000\016\000\016\000\016\000\016\000\016\000\
\016\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\052\000\019\000\019\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\054\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\060\000\025\000\025\000\025\000\025\000\025\000\025\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\061\000\027\000\027\000\027\000\027\000\027\000\
\027\000\005\000\005\000\005\000\005\000\005\000\005\000\055\000\
\058\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\059\000\017\000\017\000\017\000\022\000\
\017\000\017\000\017\000\017\000\017\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\000\000\037\000\037\000\037\000\
\037\000\014\000\015\000\037\000\037\000\037\000\037\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\000\000\000\000\
\023\000\023\000\023\000\000\000\023\000\023\000\023\000\023\000\
\023\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000\
\000\000\000\000\012\000\012\000\012\000\000\000\012\000\012\000\
\012\000\012\000\012\000\003\000\004\000\000\000\005\000\006\000\
\007\000\017\000\014\000\015\000\020\000\000\000\018\000\019\000\
\021\000\008\000\000\000\009\000\010\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\026\000\027\000\
\023\000\000\000\000\000\028\000\024\000"

let yycheck = "\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\001\000\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\013\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\003\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\010\001\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\011\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\012\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\016\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\011\001\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\012\001\017\001\018\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\012\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\018\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\048\000\013\001\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\012\001\
\012\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\012\001\010\001\011\001\012\001\006\000\
\014\001\015\001\016\001\017\001\018\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\255\255\009\001\010\001\011\001\
\012\001\007\001\008\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\255\255\255\255\
\010\001\011\001\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\001\001\002\001\255\255\004\001\005\001\
\006\001\008\001\007\001\008\001\009\001\255\255\013\001\014\001\
\013\001\015\001\255\255\017\001\018\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\008\001\009\001\
\009\001\255\255\255\255\013\001\013\001"

let yynames_const = "\
  IE\000\
  HEYA\000\
  HE\000\
  NI\000\
  KARA\000\
  SUSUMU\000\
  HAIRU\000\
  DERU\000\
  WO\000\
  DE\000\
  HASHI\000\
  WATARU\000\
  OSHIRO\000\
  EOL\000\
  "

let yynames_block = "\
  HOUKOU\000\
  ITEM\000\
  TADOUSHI\000\
  TANDOKUDOUSHI\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bun) in
    Obj.repr(
# 45 "parser.mly"
        ( _1 )
# 234 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bun) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'anys) in
    Obj.repr(
# 47 "parser.mly"
        ( raise (Error ("「" ^ _2 ^ "」？")) )
# 242 "parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'houkoujoshi) in
    Obj.repr(
# 51 "parser.mly"
        ( Idousuru (_1) )
# 250 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'houkoujoshi) in
    Obj.repr(
# 53 "parser.mly"
        ( raise (Error ("「" ^ _1 ^ "に」どうする？")) )
# 258 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
        ( raise (Error ("「" ^ _1 ^ "」にどうする？")) )
# 265 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'houkoujoshi) in
    Obj.repr(
# 57 "parser.mly"
        ( Idousuru ("入") )
# 272 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'houkoujoshi) in
    Obj.repr(
# 59 "parser.mly"
        ( raise (Error ("「家に」どうする？")) )
# 279 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
        ( Idousuru ("出") )
# 285 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
        ( raise (Error ("「家から」どうする？")) )
# 291 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
        ( Idousuru ("出") )
# 297 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
        ( raise (Error ("「家を」どうする？")) )
# 303 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
        ( raise (Error ("「家」にどうする？")) )
# 309 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
        ( Idousuru ("出") )
# 315 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
        ( raise (Error ("「部屋から」どうする？")) )
# 321 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
        ( Idousuru ("出") )
# 327 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
        ( raise (Error ("「部屋を」どうする？")) )
# 333 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
        ( raise (Error ("「部屋」にどうする？")) )
# 339 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
        ( Idousuru ("渡") )
# 345 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
        ( raise (Error ("「橋を」どうする？")) )
# 351 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
        ( raise (Error ("「橋」をどうする？")) )
# 357 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
        ( Idousuru ("入２") )
# 363 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
        ( raise (Error ("「お城に」どうする？")) )
# 369 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
        ( raise (Error ("「お城」をＯＲにどうする？")) )
# 375 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
        ( Idousuru ("出２") )
# 381 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
        ( raise (Error ("「お城から」どうする？")) )
# 387 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
        ( Idousuru ("出２") )
# 393 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
        ( raise (Error ("「お城を」どうする？")) )
# 399 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mokutekigo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
        ( Tadoushi (_1, _2) )
# 407 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mokutekigo) in
    Obj.repr(
# 104 "parser.mly"
        ( raise (Error ("「" ^ _1 ^ "を」どうする？")) )
# 414 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
        ( Tandokudoushi (_1) )
# 421 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
        ( raise (Error ("え？")) )
# 427 "parser.ml"
               : 'bun))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
        ( () (* 何も返す必要がない *) )
# 433 "parser.ml"
               : 'houkoujoshi))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
        ( () (* 何も返す必要がない *) )
# 439 "parser.ml"
               : 'houkoujoshi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 118 "parser.mly"
        ( _1 )
# 446 "parser.ml"
               : 'mokutekigo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 120 "parser.mly"
        ( _1 )
# 453 "parser.ml"
               : 'mokutekigo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 122 "parser.mly"
        ( _1 )
# 460 "parser.ml"
               : 'mokutekigo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
        ( raise (Error ("「" ^ _1 ^ "」をどうする？")) )
# 467 "parser.ml"
               : 'mokutekigo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'any) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'anys) in
    Obj.repr(
# 127 "parser.mly"
                ( _1 ^ _2 )
# 475 "parser.ml"
               : 'anys))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
                ( _1 (* 返す文字列は、エラーメッセージ用 *) )
# 482 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
                ( "家" )
# 488 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                ( "部屋" )
# 494 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                ( "橋" )
# 500 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
                ( "お城" )
# 506 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                ( "へ" )
# 512 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
                ( "に" )
# 518 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                ( "から" )
# 524 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
                ( "進む" )
# 530 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                ( "入る" )
# 536 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                ( "出る" )
# 542 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                ( "渡る" )
# 548 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
                ( "を" )
# 554 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                ( "で" )
# 560 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
                ( _1 )
# 567 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                ( _1 )
# 574 "parser.ml"
               : 'any))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 146 "parser.mly"
                ( _1 )
# 581 "parser.ml"
               : 'any))
(* Entry start *)
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
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
