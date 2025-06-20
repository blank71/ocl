%{
open Syntax
%}

%token TRUE FALSE

%token <string> VAR

%token NOT            "~"
%token AND            "&"
%token OR             "|"
%left "~" "|" "&"

%token IMPLIES        "->" 
%token IFF            "<->"
%left "<->" "->"

%token LPAREN         "("
%token RPAREN         ")"

%token EOF

%start <Syntax.expr> main

%%

main:
  | e = expr EOF 
    { e }

expr:
  | e1 = expr "<->" e2 = expr 
    { Iff (e1, e2) }
  | e1 = expr "->" e2 = expr 
    { Implies (e1, e2) }
  | e1 = expr "|" e2 = expr 
    { Or (e1, e2) }
  | e1 = expr "&" e2 = expr 
    { And (e1, e2) }
  | "~" e = expr 
    { Not e }
  | atom = atom 
    { atom }

atom:
  | TRUE { True }
  | FALSE { False }
  | x = VAR { Var x }
  | LPAREN e = expr RPAREN { e }