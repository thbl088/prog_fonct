#directory "../data/ocaml-4.08.1";;
#load "expression_scanner.cmo";;
open Expression_scanner;;

string_to_token_list "34 56 2+ x * -;";;
string_to_token_list "x 3 + 5 7 + + 3 4 * 1 3 + / /;";;
type operator =
  |Plus
  |Minus
  |Mult
  |Div
;;

type tree =
  |Var of char
  |Cst of int
  |Unary of tree
  |Binary of operator * tree * tree
;;

let rec parse (tokens : token list) : tree =
  match tl with
  |End -> continuer
  |
;;

