(* 論理式の抽象構文木 *)
type expr =
  | True                           (* 真 *)
  | False                          (* 偽 *)
  | Var of string                  (* 変数 *)
  | Not of expr                    (* 否定 *)
  | And of expr * expr             (* 論理積 *)
  | Or of expr * expr              (* 論理和 *)
  | Implies of expr * expr         (* 含意 *)
  | Iff of expr * expr             (* 双条件 *)

(* 変数の真偽値を表す環境 *)
type env = (string * bool) list

(* 論理式を文字列に変換 *)
let rec string_of_expr = function
  | True -> "true"
  | False -> "false"
  | Var x -> x
  | Not e -> "~" ^ string_of_expr e
  | And (e1, e2) -> "(" ^ string_of_expr e1 ^ " & " ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_expr e1 ^ " | " ^ string_of_expr e2 ^ ")"
  | Implies (e1, e2) -> "(" ^ string_of_expr e1 ^ " -> " ^ string_of_expr e2 ^ ")"
  | Iff (e1, e2) -> "(" ^ string_of_expr e1 ^ " <-> " ^ string_of_expr e2 ^ ")"
