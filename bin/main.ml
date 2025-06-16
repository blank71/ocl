open Ocl.Syntax
open Ocl.Eval

(* Parse with error handling *)
let parse_with_error lexbuf =
  try Ocl.Parser.main Ocl.Lexer.token lexbuf with
  | Ocl.Lexer.SyntaxError msg ->
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Ocl.Parser.Error ->
      Printf.eprintf "Parser error at position %d\n" 
        (Lexing.lexeme_start lexbuf);
      exit 1

(* Print truth table *)
let print_truth_table expr =
  let (vars, results) = truth_table expr in
  (* Header *)
  List.iter (fun var -> Printf.printf "%s\t" var) vars;
  Printf.printf "| %s\n" (string_of_expr expr);
  
  (* Separator *)
  List.iter (fun _ -> Printf.printf "---\t") vars;
  Printf.printf "| ---\n";
  
  (* Rows *)
  List.iter (fun (env, result) ->
    List.iter (fun var -> 
      let value = List.assoc var env in
      Printf.printf "%s\t" (if value then "T" else "F")
    ) vars;
    Printf.printf "| %s\n" (if result then "T" else "F")
  ) results

(* REPL *)
let rec repl () =
  Printf.printf "> %!";  (* %! forces immediate flush *)
  try
    let input = read_line () in
    if input = "quit" || input = "exit" then
      Printf.printf "Goodbye!\n"
    else if input = "" then
      repl ()
    else (
      let lexbuf = Lexing.from_string input in
      let expr = parse_with_error lexbuf in
      
      Printf.printf "Expression: %s\n" (string_of_expr expr);
      
      if is_tautology expr then
        Printf.printf "Tautology\n"
      else if is_contradiction expr then
        Printf.printf "Contradiction\n"
      else
        Printf.printf "Satisfiable\n";
      
      Printf.printf "\nTruth table:\n";
      print_truth_table expr;
      Printf.printf "\n";
      
      repl ()
    )
  with
  | End_of_file -> Printf.printf "\nGoodbye!\n"
  | exn -> 
      Printf.printf "Error: %s\n\n" (Printexc.to_string exn);
      repl ()

let () =
  Printf.printf "Logic Expression Calculator\n%!";
  Printf.printf "Operators:\n%!";
  Printf.printf "  true, false - literals\n%!";
  Printf.printf "  ~, not, !  - negation\n%!";
  Printf.printf "  &, and, && - conjunction\n%!";
  Printf.printf "  |, or, ||  - disjunction\n%!";
  Printf.printf "  ->, =>     - implication\n%!";
  Printf.printf "  <->, <=>   - biconditional\n%!";
  Printf.printf "  Variables: identifiers starting with a-z, A-Z\n%!";
  Printf.printf "Example: (p & q) -> r\n%!";
  Printf.printf "Type 'quit' or 'exit' to exit.\n\n%!";
  repl ()
