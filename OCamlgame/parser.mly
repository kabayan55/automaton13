%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token <string> HOUKOU ITEM TADOUSHI TANDOKUDOUSHI
/* これらには string 型の値が伴うことを示している */
%token IE HEYA HE NI KARA SUSUMU HAIRU DERU WO DE HASHI WATARU OSHIRO
%token EOL
/* EOL = End Of Line 入力の終わり */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start

/* 入力の文法：

文         = 方向 方向助詞 "進む"
	   | "家" 方向助詞 "入る"
	   | "家" "から" "出る"
	   | "部屋" "から" "出る"
	   | "橋" "を" "渡る"
           | 目的語 他動詞
           | 単独動詞
方向       = "東" | "西" | "南" | "北" | "北東" | "北西" | "南東" | "南西"
方向助詞   = "へ" | "に"
目的語     = アイテム "を" | "に" | "で"
アイテム   = "鍵" | "ドア" | "サボテン" | "宝" | "うさぎ" | "魚" | "しぼりたての牛乳" | "花" | "花瓶"| "牛" | "貝殻" | "バラ" | "木の実" | "ゾウ" | "釣り道具"
他動詞     = "取る" | "置く" | "ノックする" | "開く" | "閉じる" | "触る" | "撫でる" | "食べる" | "飲む" | "捕まえる"
単独動詞   = "終了する"

出力：Syntax.t 型の値

*/

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| bun EOL
        { $1 }
| bun anys EOL
        { raise (Error ("「" ^ $2 ^ "」？")) }

bun:
| HOUKOU houkoujoshi SUSUMU
        { Idousuru ($1) }
| HOUKOU houkoujoshi
        { raise (Error ("「" ^ $1 ^ "に」どうする？")) }
| HOUKOU
        { raise (Error ("「" ^ $1 ^ "」にどうする？")) }
| IE houkoujoshi HAIRU
        { Idousuru ("入") }
| IE houkoujoshi
        { raise (Error ("「家に」どうする？")) }
| IE KARA DERU
        { Idousuru ("出") }
| IE KARA
        { raise (Error ("「家から」どうする？")) }
| IE WO DERU
        { Idousuru ("出") }
| IE WO
        { raise (Error ("「家を」どうする？")) }
| IE
        { raise (Error ("「家」にどうする？")) }
| HEYA KARA DERU
        { Idousuru ("出") }
| HEYA KARA
        { raise (Error ("「部屋から」どうする？")) }
| HEYA WO DERU
        { Idousuru ("出") }
| HEYA WO
        { raise (Error ("「部屋を」どうする？")) }
| HEYA
        { raise (Error ("「部屋」にどうする？")) }
| HASHI WO WATARU
        { Idousuru ("渡") }
| HASHI WO
        { raise (Error ("「橋を」どうする？")) }
| HASHI
        { raise (Error ("「橋」をどうする？")) }
| OSHIRO NI HAIRU
        { Idousuru ("入２") }
| OSHIRO NI
        { raise (Error ("「お城に」どうする？")) }
| OSHIRO
        { raise (Error ("「お城」をＯＲにどうする？")) }
| OSHIRO KARA DERU
        { Idousuru ("出２") }
| OSHIRO KARA
        { raise (Error ("「お城から」どうする？")) }
| OSHIRO WO DERU
        { Idousuru ("出２") }
| OSHIRO WO
        { raise (Error ("「お城を」どうする？")) }

| mokutekigo TADOUSHI
        { Tadoushi ($1, $2) }
| mokutekigo
        { raise (Error ("「" ^ $1 ^ "を」どうする？")) }
| TANDOKUDOUSHI
        { Tandokudoushi ($1) }
| EOL
        { raise (Error ("え？")) }

houkoujoshi:
| HE
        { () (* 何も返す必要がない *) }
| NI
        { () (* 何も返す必要がない *) }

mokutekigo:
| ITEM WO
        { $1 }
| ITEM NI
        { $1 }
| ITEM DE
        { $1 }
| ITEM
        { raise (Error ("「" ^ $1 ^ "」をどうする？")) }

anys:
any anys        { $1 ^ $2 }

any:
| HOUKOU        { $1 (* 返す文字列は、エラーメッセージ用 *) }
| IE            { "家" }
| HEYA          { "部屋" }
| HASHI         { "橋" }
| OSHIRO        { "お城" }
| HE            { "へ" }
| NI            { "に" }
| KARA          { "から" }
| SUSUMU        { "進む" }
| HAIRU         { "入る" }
| DERU          { "出る" }
| WATARU        { "渡る" }
| WO            { "を" }
| DE            { "で" }
| ITEM          { $1 }
| TADOUSHI      { $1 }
| TANDOKUDOUSHI { $1 }
