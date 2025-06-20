%{
open Syntax
%}

%token TRUE FALSE
%token <string> VAR
%token NOT AND OR IMPLIES IFF
%token LPAREN RPAREN
%token EOF

%left IFF
%left IMPLIES
%left OR
%left AND
%right NOT

%type <Syntax.expr> main
%start main

%%

main:
  | expr EOF { $1 }

expr:
  | expr IFF expr { Iff ($1, $3) }
  | expr IMPLIES expr { Implies ($1, $3) }
  | expr OR expr { Or ($1, $3) }
  | expr AND expr { And ($1, $3) }
  | NOT expr { Not $2 }
  | atom { $1 }

atom:
  | TRUE { True }
  | FALSE { False }
  | VAR { Var $1 }
  | LPAREN expr RPAREN { $2 }