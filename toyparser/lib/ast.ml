type ast =
    Const of int
  | Add of ast * ast
  | Sub of ast * ast

