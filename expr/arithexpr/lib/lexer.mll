{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "iszero" { ISZERO }
  | "pred" { PRED }
  | "succ" { SUCC }
  | "0" { ZERO }
  | eof { EOF }