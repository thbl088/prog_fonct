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
  |var1::var2::tl_pile ->( Binary(ope, var2, var1))::tl_pile
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

let tree_test : tree = transf_in_tree(string_to_token_list("1 ~ 2 +"), []);;
let tree_test : tree = transf_in_tree(string_to_token_list ("x 3 + 5 7 + + 3 4 * 1 3 + / /"), []);;
tree_test;;

let rec parcours_infixe (tree: tree) : string =
  match tree with
  |Var(f)       -> Char.escaped f
  |Cst(f)       -> Int.to_string f
  |Unary(t)     -> "(-" ^ (parcours_infixe t) ^ ")"
  |Binary(n,l,r)->
     match n with
     |Plus -> "(" ^ parcours_infixe (l) ^ "+" ^ parcours_infixe (r) ^ ")"
     |Minus-> "(" ^ parcours_infixe (l) ^ "-" ^ parcours_infixe (r) ^ ")"
     |Mult -> "(" ^ parcours_infixe (l) ^ "*" ^ parcours_infixe (r) ^ ")"
     |Div  -> "(" ^ parcours_infixe (l) ^ "/" ^ parcours_infixe (r) ^ ")"
;;

let rec parcours_postfixe(tree : tree) : tree =
  match tree with
  |Binary(Plus,Cst(var1), Cst(var2)) -> Cst(var1+var2)
  |Binary(Minus,Cst(var1), Cst(var2)) -> Cst(var1-var2)
  |Binary(Div,Cst(var1), Cst(var2)) -> Cst(var1/var2)
  |Binary(Mult, Cst(var1), Cst(var2)) -> Cst(var1*var2)
  |Binary(o,l,r) -> Binary(o, parcours_postfixe(l), parcours_postfixe(r))
  |Unary(_) -> tree
  |Var(_) -> tree
  |Cst(_) -> tree
;;



let rec affich_simp (tree: tree) : string =
  match tree with
  |Var(f)       -> Char.escaped f
  |Cst(f)       -> Int.to_string f
  |Unary(t)     -> "(-" ^ (parcours_infixe t) ^ ")"
  |Binary(n,l,r)->
     match n with
     |Plus -> "(" ^ affich_simp(parcours_postfixe(l)) ^ "+" ^ affich_simp(parcours_postfixe(r)) ^ ")"
     |Minus-> "(" ^ affich_simp(parcours_postfixe(l)) ^ "-" ^ affich_simp(parcours_postfixe(r)) ^ ")"
     |Mult -> "(" ^ affich_simp(parcours_postfixe(l)) ^ "*" ^ affich_simp(parcours_postfixe(r)) ^ ")"
     |Div  -> "(" ^ affich_simp(parcours_postfixe(l)) ^ "/" ^ affich_simp(parcours_postfixe(r)) ^ ")"
;;
parcours_postfixe(tree_test);;
parcours_infixe(tree_test);;


(*let tree_test : tree = transf_in_tree(string_to_token_list("1 0 *"), []);; *)

  
affich_simp(tree_test);
