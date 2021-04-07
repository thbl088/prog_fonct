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

let aux_transf_in_Untree(pile : tree list): tree list=
  match pile with
  |var1::tl_pile -> (Unary(var1)::tl_pile)
  |[] -> failwith "manque un chiffre ou une constante"
;;


let aux_transf_in_Betree( pile , ope : tree list * operator): tree list =
  match pile with
  |var1::var2::tl_pile ->( Binary(ope, var1, var2))::tl_pile
  |[] -> failwith "manque un chiffre ou une constante"
;;

let token_transf_in_tree( token, pile : token * tree list) : tree list =
  match token  with 
  |Variable(var) -> Var(var)::pile
  |Number(var) -> Cst(var)::pile
  |Add-> aux_transf_in_Betree(pile , Plus)
  |Subtract -> aux_transf_in_Betree(pile, Minus);
  |Minus-> aux_transf_in_Untree(pile) 
  |Multiply-> aux_transf_in_Betree(pile , Mult)  
  |Divide  -> aux_transf_in_Betree(pile , Div)
  |End -> []
;;

let rec transf_in_tree ( tokens, pile : token list * tree list) : tree =
  match tokens with
  |[] -> List.hd (pile)
  |head::tail -> transf_in_tree(tail, token_transf_in_tree(head, pile)) 

;;

transf_in_tree(string_to_token_list("1 ~ 2 +"), []);;



                     
                            
let rec parcours_infixe f1 f2 = function
| Feuille x -> f1 x
| Noeud(x,g,d) -> parcours_infixe f1 f2 g ;
f2 x ;
parcours_infixe f1 f2 d;;

let affichage_non_simplifier (pile : tree list) =
  match pile with
  |head::tail -> parcours_infixe head pile;
;;
