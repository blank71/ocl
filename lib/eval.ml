open Syntax

(* 環境から変数の値を取得 *)
let rec lookup env x =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y, v) :: rest -> if x = y then v else lookup rest x

(* 論理式を評価 *)
let rec eval env = function
  | True -> true
  | False -> false
  | Var x -> lookup env x
  | Not e -> not (eval env e)
  | And (e1, e2) -> (eval env e1) && (eval env e2)
  | Or (e1, e2) -> (eval env e1) || (eval env e2)
  | Implies (e1, e2) -> (not (eval env e1)) || (eval env e2)
  | Iff (e1, e2) -> (eval env e1) = (eval env e2)

(* 論理式で使用される変数を収集 *)
let rec collect_vars = function
  | True | False -> []
  | Var x -> [x]
  | Not e -> collect_vars e
  | And (e1, e2) | Or (e1, e2) | Implies (e1, e2) | Iff (e1, e2) -> (collect_vars e1) @ (collect_vars e2)

(* 重複を除去 *)
let unique_vars vars =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> if List.mem x acc then aux acc xs else aux (x :: acc) xs
  in
  aux [] vars

(* 真理値表を生成 *)
let truth_table expr =
  let vars = unique_vars (collect_vars expr) in
  let rec generate_assignments vars =
    match vars with
    | [] -> [[]]
    | x :: xs ->
        let rest = generate_assignments xs in
        List.flatten [
          List.map (fun env -> (x, false) :: env) rest;
          List.map (fun env -> (x, true) :: env) rest
        ]
  in
  let assignments = generate_assignments vars in
  (vars, List.map (fun env -> (env, eval env expr)) assignments)

(* 論理式が恒真か判定 *)
let is_tautology expr =
  let (_, results) = truth_table expr in
  List.for_all (fun (_, result) -> result) results

(* 論理式が矛盾か判定 *)
let is_contradiction expr =
  let (_, results) = truth_table expr in
  List.for_all (fun (_, result) -> not result) results
