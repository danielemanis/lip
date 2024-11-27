%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token OR
%token AND
%token NOT
%token ZERO
%token ISZERO
%token PRED
%token SUCC


%nonassoc ELSE
%left OR
%left AND 
%left NOT 
%left ISZERO
%left PRED SUCC 


%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | e1=expr; AND; e2=expr; { And(e1,e2) }  
  | e1=expr; OR; e2=expr; { Or(e1,e2) }  
  | NOT; e1= expr; { Not(e1)}
  | ZERO { Zero }
  | ISZERO; e1=expr; { IsZero(e1) }
  | PRED; e1=expr;  { Pred(e1) }
  | SUCC; e1=expr;  { Succ(e1) }
;
