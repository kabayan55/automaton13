(* ����黻�Ҥη� *)
type op2_t = Minus | Times | Plus | Divide | Mod | Power

(* ñ��黻�Ҥη� *)
type op1_t = UMinus | UPlus

(* Syntax.t: parser �����Ϥ�����ݹ�ʸ�ڤη� *)
type t  = Num of int
	| Op2 of op2_t * t * t
	| Op1 of op1_t * t

(* Syntax.print: ��ݹ�ʸ�ڤ�ץ��Ȥ���ؿ� *)

(* string_of_expr : Syntax.t -> string *)
let rec string_of_expr expr = match expr with
    Num (n) -> string_of_int n
  | Op2 (op, arg1, arg2) ->
	"(" ^ string_of_expr arg1
	    ^ (match op with
		  Minus -> "-"
		| Times -> "*"
		| Plus -> "+"
		| Divide -> "/"
		| Mod -> "mod"
		| Power -> "^")
	    ^ string_of_expr arg2 ^ ")"
  | Op1 (op, arg) ->
	(match op with
	    UMinus -> "(-"
	   |UPlus -> "(+")
 
	^ string_of_expr arg ^ ")"

(* print : Syntax.t -> unit *)
let print expr =
  let str = string_of_expr expr
  in print_string str
