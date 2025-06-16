open Ocl.Syntax
open Ocl.Eval

(* パーサーのヘルパー関数 *)
let parse_expr str =
  let lexbuf = Lexing.from_string str in
  try
    Some (Ocl.Parser.main Ocl.Lexer.token lexbuf)
  with
  | _ -> None

(* テスト用のヘルパー関数 *)
let test_parse_success name input expected =
  let test_case () =
    match parse_expr input with
    | Some expr -> Alcotest.(check bool) "parsed expression" true (expr = expected)
    | None -> Alcotest.fail ("Failed to parse: " ^ input)
  in
  (name, `Quick, test_case)

let test_parse_failure name input =
  let test_case () =
    match parse_expr input with
    | Some _ -> Alcotest.fail ("Should not parse: " ^ input)
    | None -> Alcotest.(check bool) "parse failure" true true
  in
  (name, `Quick, test_case)

let test_eval name expr env expected =
  let test_case () =
    let result = eval env expr in
    Alcotest.(check bool) ("evaluate " ^ name) expected result
  in
  (name, `Quick, test_case)

let test_tautology name expr expected =
  let test_case () =
    let result = is_tautology expr in
    Alcotest.(check bool) ("tautology " ^ name) expected result
  in
  (name, `Quick, test_case)

let test_contradiction name expr expected =
  let test_case () =
    let result = is_contradiction expr in
    Alcotest.(check bool) ("contradiction " ^ name) expected result
  in
  (name, `Quick, test_case)

(* 基本パーサーのテスト *)
let parser_tests = [
  test_parse_success "parse true" "true" True;
  test_parse_success "parse false" "false" False;
  test_parse_success "parse variable" "p" (Var "p");
  test_parse_success "parse negation" "~p" (Not (Var "p"));
  test_parse_success "parse not" "not p" (Not (Var "p"));
  test_parse_success "parse and" "p & q" (And (Var "p", Var "q"));
  test_parse_success "parse and keyword" "p and q" (And (Var "p", Var "q"));
  test_parse_success "parse or" "p | q" (Or (Var "p", Var "q"));
  test_parse_success "parse or keyword" "p or q" (Or (Var "p", Var "q"));
  test_parse_success "parse implies" "p -> q" (Implies (Var "p", Var "q"));
  test_parse_success "parse implies alt" "p => q" (Implies (Var "p", Var "q"));
  test_parse_success "parse iff" "p <-> q" (Iff (Var "p", Var "q"));
  test_parse_success "parse iff alt" "p <=> q" (Iff (Var "p", Var "q"));
  test_parse_success "parse parentheses" "(p & q)" (And (Var "p", Var "q"));
  test_parse_success "parse complex" "(p & q) -> r" 
    (Implies (And (Var "p", Var "q"), Var "r"));
  test_parse_success "parse nested" "~(p | q)" 
    (Not (Or (Var "p", Var "q")));
  test_parse_failure "parse invalid" "p &";
  test_parse_failure "parse unmatched paren" "(p & q";
]

(* 基本評価のテスト *)
let evaluation_tests = [
  test_eval "true evaluation" True [] true;
  test_eval "false evaluation" False [] false;
  test_eval "variable true" (Var "p") [("p", true)] true;
  test_eval "variable false" (Var "p") [("p", false)] false;
  test_eval "not true" (Not True) [] false;
  test_eval "not false" (Not False) [] true;
  test_eval "and true true" (And (True, True)) [] true;
  test_eval "and true false" (And (True, False)) [] false;
  test_eval "and false true" (And (False, True)) [] false;
  test_eval "and false false" (And (False, False)) [] false;
  test_eval "or true true" (Or (True, True)) [] true;
  test_eval "or true false" (Or (True, False)) [] true;
  test_eval "or false true" (Or (False, True)) [] true;
  test_eval "or false false" (Or (False, False)) [] false;
  test_eval "implies true true" (Implies (True, True)) [] true;
  test_eval "implies true false" (Implies (True, False)) [] false;
  test_eval "implies false true" (Implies (False, True)) [] true;
  test_eval "implies false false" (Implies (False, False)) [] true;
  test_eval "iff true true" (Iff (True, True)) [] true;
  test_eval "iff true false" (Iff (True, False)) [] false;
  test_eval "iff false true" (Iff (False, True)) [] false;
  test_eval "iff false false" (Iff (False, False)) [] true;
  test_eval "complex expression" 
    (And (Var "p", Implies (Var "q", Var "r")))
    [("p", true); ("q", true); ("r", false)] false;
]

(* 論理法則のテスト *)
let logic_tests = [
  test_tautology "p or not p" (Or (Var "p", Not (Var "p"))) true;
  test_tautology "not (p and not p)" (Not (And (Var "p", Not (Var "p")))) true;
  test_tautology "p implies p" (Implies (Var "p", Var "p")) true;
  test_tautology "double negation" 
    (Iff (Var "p", Not (Not (Var "p")))) true;
  test_tautology "modus ponens valid" 
    (Implies (And (Var "p", Implies (Var "p", Var "q")), Var "q")) true;
  test_tautology "not tautology" (Var "p") false;
  test_tautology "not tautology and" (And (Var "p", Var "q")) false;
  
  test_contradiction "p and not p" (And (Var "p", Not (Var "p"))) true;
  test_contradiction "false" False true;
  test_contradiction "not contradiction" True false;
  test_contradiction "not contradiction var" (Var "p") false;
]

(* 変数収集のテスト *)
let collect_vars_tests = [
  ("empty vars for true", `Quick, fun () ->
    let vars = collect_vars True in
    Alcotest.(check (list string)) "no variables" [] vars);
  
  ("single variable", `Quick, fun () ->
    let vars = collect_vars (Var "p") in
    Alcotest.(check (list string)) "single var" ["p"] vars);
  
  ("multiple variables", `Quick, fun () ->
    let vars = collect_vars (And (Var "p", Var "q")) in
    let expected = ["p"; "q"] in
    Alcotest.(check (list string)) "multiple vars" expected vars);
  
  ("duplicate variables", `Quick, fun () ->
    let vars = unique_vars (collect_vars (And (Var "p", Var "p"))) in
    Alcotest.(check (list string)) "unique vars" ["p"] vars);
]

(* De Morgan の法則のテスト *)
let de_morgan_tests = [
  ("de morgan and", `Quick, fun () ->
    let expr1 = Not (And (Var "p", Var "q")) in
    let expr2 = Or (Not (Var "p"), Not (Var "q")) in
    let (_, results1) = truth_table expr1 in
    let (_, results2) = truth_table expr2 in
    let equivalent = List.for_all2 (fun (_, r1) (_, r2) -> r1 = r2) results1 results2 in
    Alcotest.(check bool) "de morgan and equivalent" true equivalent);
  
  ("de morgan or", `Quick, fun () ->
    let expr1 = Not (Or (Var "p", Var "q")) in
    let expr2 = And (Not (Var "p"), Not (Var "q")) in
    let (_, results1) = truth_table expr1 in
    let (_, results2) = truth_table expr2 in
    let equivalent = List.for_all2 (fun (_, r1) (_, r2) -> r1 = r2) results1 results2 in
    Alcotest.(check bool) "de morgan or equivalent" true equivalent);
]

(* 含意の性質のテスト *)
let implication_tests = [
  ("contrapositive", `Quick, fun () ->
    let expr1 = Implies (Var "p", Var "q") in
    let expr2 = Implies (Not (Var "q"), Not (Var "p")) in
    let test_cases = [
      ([("p", false); ("q", false)], true);
      ([("p", false); ("q", true)], true);
      ([("p", true); ("q", false)], false);
      ([("p", true); ("q", true)], true);
    ] in
    let all_equivalent = List.for_all (fun (env, expected) ->
      let result1 = eval env expr1 in
      let result2 = eval env expr2 in
      result1 = result2 && result1 = expected
    ) test_cases in
    Alcotest.(check bool) "contrapositive equivalent" true all_equivalent);
  
  ("implication as disjunction", `Quick, fun () ->
    let expr1 = Implies (Var "p", Var "q") in
    let expr2 = Or (Not (Var "p"), Var "q") in
    let (_, results1) = truth_table expr1 in
    let (_, results2) = truth_table expr2 in
    let equivalent = List.for_all2 (fun (_, r1) (_, r2) -> r1 = r2) results1 results2 in
    Alcotest.(check bool) "implication as disjunction" true equivalent);
]

(* より高度な論理法則のテスト *)
let advanced_logic_tests = [
  (* 分配法則のテスト *)
  ("distributive law and over or", `Quick, fun () ->
    let expr1 = And (Var "p", Or (Var "q", Var "r")) in
    let expr2 = Or (And (Var "p", Var "q"), And (Var "p", Var "r")) in
    let test_cases = [
      ([("p", false); ("q", false); ("r", false)], false);
      ([("p", false); ("q", false); ("r", true)], false);
      ([("p", false); ("q", true); ("r", false)], false);
      ([("p", false); ("q", true); ("r", true)], false);
      ([("p", true); ("q", false); ("r", false)], false);
      ([("p", true); ("q", false); ("r", true)], true);
      ([("p", true); ("q", true); ("r", false)], true);
      ([("p", true); ("q", true); ("r", true)], true);
    ] in
    let all_equivalent = List.for_all (fun (env, expected) ->
      let result1 = eval env expr1 in
      let result2 = eval env expr2 in
      result1 = result2 && result1 = expected
    ) test_cases in
    Alcotest.(check bool) "distributive law" true all_equivalent);

  (* 結合法則のテスト *)
  ("associative law and", `Quick, fun () ->
    let expr1 = And (And (Var "p", Var "q"), Var "r") in
    let expr2 = And (Var "p", And (Var "q", Var "r")) in
    let test_cases = [
      ([("p", false); ("q", false); ("r", false)], false);
      ([("p", false); ("q", false); ("r", true)], false);
      ([("p", false); ("q", true); ("r", false)], false);
      ([("p", false); ("q", true); ("r", true)], false);
      ([("p", true); ("q", false); ("r", false)], false);
      ([("p", true); ("q", false); ("r", true)], false);
      ([("p", true); ("q", true); ("r", false)], false);
      ([("p", true); ("q", true); ("r", true)], true);
    ] in
    let all_equivalent = List.for_all (fun (env, expected) ->
      let result1 = eval env expr1 in
      let result2 = eval env expr2 in
      result1 = result2 && result1 = expected
    ) test_cases in
    Alcotest.(check bool) "associative law and" true all_equivalent);

  (* 三段論法のテスト *)
  ("syllogism", `Quick, fun () ->
    let premise1 = Implies (Var "p", Var "q") in
    let premise2 = Implies (Var "q", Var "r") in
    let conclusion = Implies (Var "p", Var "r") in
    let syllogism = Implies (And (premise1, premise2), conclusion) in
    let is_valid = is_tautology syllogism in
    Alcotest.(check bool) "syllogism valid" true is_valid);
]

(* 真理値表の完全性テスト *)
let truth_table_tests = [
  ("truth table completeness 1 var", `Quick, fun () ->
    let expr = Var "p" in
    let (vars, results) = truth_table expr in
    Alcotest.(check int) "1 variable table size" 2 (List.length results);
    Alcotest.(check (list string)) "1 variable names" ["p"] vars);

  ("truth table completeness 2 vars", `Quick, fun () ->
    let expr = And (Var "p", Var "q") in
    let (vars, results) = truth_table expr in
    Alcotest.(check int) "2 variable table size" 4 (List.length results);
    Alcotest.(check int) "2 variable count" 2 (List.length vars));

  ("truth table completeness 3 vars", `Quick, fun () ->
    let expr = And (And (Var "p", Var "q"), Var "r") in
    let (vars, results) = truth_table expr in
    Alcotest.(check int) "3 variable table size" 8 (List.length results);
    Alcotest.(check int) "3 variable count" 3 (List.length vars));
]

(* エラーハンドリングのテスト *)
let error_handling_tests = [
  ("unbound variable error", `Quick, fun () ->
    let expr = Var "undefined" in
    let env = [("p", true); ("q", false)] in
    Alcotest.check_raises "unbound variable" 
      (Failure "Unbound variable: undefined")
      (fun () -> ignore (eval env expr)));
]

(* 統合テストスイート *)
let () =
  let open Alcotest in
  run "Logic Expression Calculator - Complete Test Suite" [
    ("Parser", parser_tests);
    ("Evaluation", evaluation_tests);
    ("Logic Properties", logic_tests);
    ("Variable Collection", collect_vars_tests);
    ("De Morgan's Laws", de_morgan_tests);
    ("Implication Properties", implication_tests);
    ("Advanced Logic Laws", advanced_logic_tests);
    ("Truth Table Completeness", truth_table_tests);
    ("Error Handling", error_handling_tests);
  ]
