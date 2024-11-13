%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS


%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) } 
  | e1 = expr; MINUS; e2 = expr { Mul(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
