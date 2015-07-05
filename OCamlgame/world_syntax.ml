(* (̾��, �ͤΥꥹ��) *)
type pair_t = string * string list

(* subsection (---̾��---) �η� *)
type subsection_t = string * pair_t list

(* section (===̾��===) �η� *)
type section_content_t = {
  initial_pair : pair_t;
  subsections : subsection_t list;
}

type section_t = string * section_content_t

(* world_parser ���֤���ʸ�ڤη� *)
type t = {
  messages : string list;
  sections : section_t list;
  shuryo_jouken : pair_t list;
}

(* �ʲ���World_syntax.t ���ͤ���������ͤ���Ф��ؿ��� *)
let extract_shoki_messages { messages = ms } = ms

let extract_shuppatsuten { sections = lst } =
  let section = List.assoc "���" lst in
  match section.initial_pair with
      ("������������", [place]) -> place
    | ("������������", _::_) ->
	failwith "��������������������ʾ塢���ꤵ��Ƥ��ޤ���"
    | _ ->
	failwith "�����������������ꤵ��Ƥ��ޤ���"

let extract_shoki_items { sections = lst } =
  let section = List.assoc "�����ƥ�" lst in
  match section.initial_pair with
      ("��������ƥ�", items) -> items
    | _ -> []

let extract_chizu_list { sections = lst } =
  let section = List.assoc "���" lst in
  List.map (fun (name, pairs) ->
	     (name,
	      let pairs2 = List.filter (fun (name, _) -> name <> "�����ƥ�")
				       pairs in
	      List.map (function (name, []) ->
				   failwith (name ^ "�ιԤ��褬����ޤ���")
			       | (name, [s]) -> (name, s)
			       | (name, _::_) ->
				   failwith (name ^ "�ιԤ��褬¿�᤮�ޤ���"))
		       pairs2))
	   section.subsections

let extract_place_state { sections = lst } =
  let section = List.assoc "���" lst in
  List.map (fun (name, pairs) ->
	      (name, ref (try
			    List.assoc "�����ƥ�" pairs
			  with Not_found -> [])))
	   section.subsections

let extract_dousa_list { sections = lst } action_list message_fun =
  let section = List.assoc "�����ƥ�" lst in
  List.map (fun (name, pairs) ->
	      (name,
	       List.map (function (n, []) ->
				    let action = List.assoc n action_list in
				    (n, action name)
			        | (n, [s]) -> (n, message_fun s)
			        | (n, _) ->
				    failwith "���������¿�᤮�ޤ���")
			 pairs))
	   section.subsections

let extract_shuryo_basho { shuryo_jouken = lst } =
  try
    match List.assoc "���" lst with
        [] -> failwith "��λ��꤬���ꤵ��Ƥ��ޤ���"
      | [basho] -> basho
      | _ :: _ -> failwith "��λ��꤬¿�᤮�ޤ���"
  with Not_found -> failwith "��λ��꤬���ꤵ��Ƥ��ޤ���"

let extract_shuryo_items { shuryo_jouken = lst } =
  try
    List.assoc "�����ƥ�" lst
  with Not_found -> failwith "��λ�����ƥब���ꤵ��Ƥ��ޤ���"

let extract_shuryo_messages { shuryo_jouken = lst } =
  try
    List.assoc "��å�����" lst
  with Not_found -> failwith "��λ��å����������ꤵ��Ƥ��ޤ���"
