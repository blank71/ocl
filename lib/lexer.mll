{
open Parser

exception SyntaxError of string
}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | whitespace+ { token lexbuf }
  | newline { token lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "~" | "not" | "!" { NOT }
  | "&" | "and" | "&&" { AND }
  | "|" | "or" | "||" { OR }
  | "->" | "=>" | "implies" { IMPLIES }
  | "<->" | "<=>" | "iff" { IFF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id as str { VAR str }
  | eof { EOF }
  | _ as c { raise (SyntaxError ("Unexpected character: " ^ String.make 1 c)) }
