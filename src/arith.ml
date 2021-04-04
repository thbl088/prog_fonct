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



let aux_transf_in_tree( pile , ope : token list * operator): tree =
  match pile with
  |var1::var2::tl_pile -> transf_in_tree ((tail, maketree(var1, vare2, ope)::tl_pile)
;;
  

let rec transf_in_tree ( tokens, pile : token list * tree list) : tree =
  match tokens with
  |[] -> List.hd (pile)
  |head::tail -> sous_transf_in_tree(head, pile);

;;


let sous_transf_in_tree( token, pile : token * tree list) : tree =
  match token  with 
  |Var -> transf_in_tree(tail, var::pile);                 
  |Plus-> aux_transf_in_tree(pile , Plus);
  |Minus-> aux_transf_in_tree(pile , Minus); 
  |Mult -> aux_transf_in_tree(pile , Mult);  
  |Div  -> aux_transf_in_tree(pile , Div);
;;
                     
                            
  
    
