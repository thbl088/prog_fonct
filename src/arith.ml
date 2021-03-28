#directory "../data/ocaml-4.08.1";;
#load "expression_scanner.cmo";;
open Expression_scanner;;

string_to_token_list "34 56 2+ x * -;";;
