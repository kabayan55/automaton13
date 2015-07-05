open Syntax
open World_syntax

(* �⡢���Ӥξ��֤�ɽ���� *)
type door_state_t = Locked | Open | Closed
type vase_state_t = Empty | Put | Full
type fishing_state_t = Unused | Using | Used

(* ������ξ��֡������ˤ�ɽ���� *)
type state_t = {
  mutable place : string;				 (* ���ߡ������� *)
  mutable items : string list;				 (* ����ʪ�ꥹ�� *)
	  place_state : (string * string list ref) list; (* �ƾ��ˤ���ʪ *)
  mutable door_state : door_state_t;			 (* �ɥ��ξ��� *)
  mutable vase_state : vase_state_t;			 (* ���Ӥξ��� *)
  mutable fishing_state : fishing_state_t;			 (* ���ƻ��ξ��� *)
  mutable hp : int;				(* �ҥåȥݥ���ȡ��Ի��ѡ� *)
}

(* ��Ū����ư���ޥ�ɤ�������� *)
(* idou : state_t -> string -> chizu_list -> unit *)
let idou state houkou chizu_list =
  if (houkou = "��" || houkou = "��") && state.door_state <> Open then
    print_endline "�⤬�ĤޤäƤ��ޤ���"
  else try
    let new_place = List.assoc houkou (List.assoc state.place chizu_list) in
    state.place <- new_place
  with Not_found -> print_endline "�����ˤϹԤ���ޤ��󡣡��"


(* wataru : state_t -> string -> chizu_list -> unit *)
let wataru state houkou chizu_list =
  if (houkou = "��") then
    try
    let new_place = List.assoc houkou (List.assoc state.place chizu_list) in
    state.place <- new_place
  with Not_found -> print_endline "�����ˤϹԤ���ޤ��󡣡��"





(* �ʲ���ư����������ؿ��� *)

(* ��Ū����å�������ɽ������ *)
(* message : string -> state_t -> unit *)
let message str state = print_endline str

(* ��Ū���ּ��פ�������� *)
(* toru : string -> state_t -> unit *)
let toru item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ��󡣡��")
  else if List.mem item state.items
    then print_endline ("���ʤ��Ϥ��Ǥ�" ^ item ^ "����äƤ��롣")
  else (state.items <- item :: state.items;
	r := List.filter (fun i -> i <> item) !r;
	print_endline ("���ʤ���" ^ item ^ "�������줿���ʡʡ���ա�ˤġˡˡ�"))

(* ��Ū���ֿ��٤�פ�������� *)
(* taberu : string -> state_t -> unit *)
let taberu item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else if List.mem item state.items
    then print_endline ("���ʤ��Ϥ��Ǥ�" ^ item ^ "�򿩤٤���")
  else (state.items <- item :: state.items;
	r := List.filter (fun i -> i <> item) !r;
	print_endline ("���ʤ���" ^ item ^ "�򿩤٤��������ˤʤä��衪Ц(���ϡ�)��"))

(* ��Ū���ְ���פ�������� *)
(* nomu : string -> state_t -> unit *)
let nomu item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else if List.mem item state.items
    then print_endline ("���ʤ��Ϥ��Ǥ�" ^ item ^ "��������")
  else (state.items <- item :: state.items;
	r := List.filter (fun i -> i <> item) !r;
	print_endline ("���ʤ���" ^ item ^ "�����������������äƤ����碌��Ц(���ϡ�)��"))


(* ��Ū�����֤��פ�������� *)
(* oku : string -> state_t -> unit *)
let oku item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item state.items)
    then print_endline ("���ʤ���" ^ item ^ "����äƤ��ʤ���")
  else (state.items <- List.filter (fun x -> x <> item) state.items;
	r := item :: !r;
	print_endline ("���ʤ���" ^ item ^ "���֤�����"))

(* ��Ū���ֳ����פ�������� *)
(* hiraku : string -> state_t -> unit *)
let hiraku item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else match state.door_state with
      Locked -> if List.mem "��" state.items
	        then (state.door_state <- Open;
		      print_endline ("���ʤ�����򳫤�����"))
	        else print_endline ("��ϻܾ�����Ƥ��롣")
    | Open   -> print_endline ("��Ϥ��Ǥ˳����Ƥ��롣")
    | Closed -> (state.door_state <- Open;
	         print_endline ("���ʤ�����򳫤�����"))

(* ��Ū�����Ĥ���פ�������� *)
(* tojiru : string -> state_t -> unit *)
let tojiru item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else match state.door_state with
      Locked -> print_endline ("��Ϥ��Ǥ��ĤޤäƤ��롣")
    | Open   -> state.door_state <- Closed;
	        print_endline ("���ʤ�������Ĥ᤿��")
    | Closed -> print_endline ("��Ϥ��Ǥ��ĤޤäƤ��롣")


(* ��Ū����������פ�������� *)
(* ikeru : string -> state_t -> unit *)
let ikeru item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else match state.vase_state with
      Empty -> if List.mem "��" state.items
	        then (state.vase_state <- Put;
		      print_endline ("���ʤ��ϲ֤򤤤�����"))
	        else print_endline ("�֤�����ޤ���")
    | Put  -> print_endline ("���ʤ��Ϥ��Ǥ˲֤򤤤��Ƥ��롣")
    | Full -> (state.vase_state <- Put;
	         print_endline ("���ʤ��ϲ֤򤤤�����"))

(* ��Ū�������פ�������� *)
(* tsuru : string -> state_t -> unit *)
let tsuru item state =
  let r = List.assoc state.place state.place_state in
  (*if not (List.mem item !r)
    then print_endline ("������" ^ item ^ "�Ϥ���ޤ���")
  else*) match state.fishing_state with
      Unused -> if List.mem "���ƻ��" state.items
	        then (state.fishing_state <- Using;
		      print_endline ("���ʤ��ϵ�����ä���"))
	        else print_endline ("���ƻ�񤬤���ޤ���")
    | Using  -> print_endline ("���ʤ��Ϥ��Ǥ˵�����äƤ��롣")
    | Used -> (state.fishing_state <- Using;
	         print_endline ("���ʤ��ϵ�����ä���"))



(* ��Ū������ʸ�˽��ä�ư���Ԥ� *)
(* dispatch : Syntax.t -> state_t -> dousa_list -> chizu_list -> unit *)
let dispatch input state dousa_list chizu_list = match input with
    Idousuru (houkou) -> idou state houkou chizu_list
  | Tadoushi (mokutekigo, tadoushi) ->
      let lst = List.assoc mokutekigo dousa_list in
		(* ������Ū��˻Ȥ���ư��Υꥹ�Ȥ����� *)
      (try
	 let thunk = List.assoc tadoushi lst in
	 thunk state (* ư���¹� *)
       with Not_found ->
	     print_endline (mokutekigo ^ "��" ^
			    tadoushi ^ "���ȤϤǤ��ޤ���"))
  | Tandokudoushi ("��λ����") ->
      print_endline "�ޤ�ͷ��Ǥ͡�"; exit 0
  | Tandokudoushi (tandokudoushi) ->
      print_endline (tandokudoushi ^ "���ȤϤǤ��ޤ���")

(* ��Ū�������Ϥξ����ɽ������ *)
(* basho_message : state_t -> unit *)
let basho_message state =
  print_endline ("���ʤ���" ^ state.place ^ "�ˤ��롣");
  print_string "�����ˤ�";
  match !(List.assoc state.place state.place_state) with
      [] -> print_endline "����ʤ���"
    | item :: rest ->
	print_string item;

	if (item = "������")
	then print_endline "�����롣"
	else if (item = "����")
	then print_endline "�����롣"
	else if (item ="��")
	then print_endline "�����롣"
	else(
	  List.iter (fun item ->
		       print_string "��";
		       print_string item)
	    rest;
	  
	  print_endline "�����롣")

(* ��Ū��������Υᥤ��롼�� *)
(* loop : state_t -> ... -> 'a *)
let rec loop state dousa_list chizu_list
	     shuryo_basho shuryo_items shuryo_messages =
  if state.place = shuryo_basho &&	(* ��λ���ˤ��� *)
     List.fold_right (fun item b -> List.mem item state.items && b)
		     shuryo_items true	(* ��λ�����ƥ�����ƻ��äƤ����� *)
  then (List.iter print_endline shuryo_messages; (* ��λ��å�����������ɽ�� *)
	exit 0);
  basho_message state;
  print_string "> ";
  let line = read_line () in			(* �����ɤ߹��ߡ�*)
  (try						(* ������ϡ���ʸ���Ϥ� *)
     let input = Parser.start Lexer.token (Lexing.from_string line) in
     dispatch input state dousa_list chizu_list	(* ư���������� *)
   with Error (str) -> print_endline str
      | Not_found -> print_endline "���á��ʡ��ϡ���"
      | Parsing.Parse_error -> print_endline "�����á��ʡ��ϡ���");
  print_newline ();
  loop state dousa_list chizu_list shuryo_basho shuryo_items shuryo_messages

(* ������γ��� *)
let _ = try
  let world = World_parser.start World_lexer.token
				 (Lexing.from_channel (open_in "world.txt")) in
  let messages = extract_shoki_messages world in
  List.iter print_endline messages; (* �����å�������ɽ�� *)
  print_newline ();
  (* ������ν������ *)
  let init_state = {
    place = extract_shuppatsuten world;
    items = extract_shoki_items world;
    place_state = extract_place_state world;
    door_state = Locked;
    vase_state = Empty;
    fishing_state = Unused;
    hp = 100;
  } in
  (* �����������б�ɽ *)
  let action_list = [
    ("���", toru); ("�֤�", oku); ("����", hiraku); ("�Ĥ���", tojiru);  ("���٤�", taberu);  ("����", nomu); ("������", ikeru);  ("���", tsuru);
  ] in
  (* ư�� *)
  let dousa_list = extract_dousa_list world action_list message in
  (* �Ͽ� *)
  let chizu_list = extract_chizu_list world in
  (* ��λ��� *)
  let shuryo_basho = extract_shuryo_basho world in
  (* ��λ�����ƥ� *)
  let shuryo_items = extract_shuryo_items world in
  (* ��λ��å����� *)
  let shuryo_messages = extract_shuryo_messages world in
  loop init_state dousa_list chizu_list
       shuryo_basho shuryo_items shuryo_messages
with Sys_error (str) ->
  failwith "world.txt �����Ĥ���ޤ��󡣡��"
