open Expression_scanner;;

(****************************************************
                   Types
 ****************************************************)
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

(****************************************************
                   Functions transf_in_tree
 ****************************************************)
(*Transforme le premier arbre d'une liste d'arbre et un operateur en arbre unaire et retourne le reste de la liste avec le nouvel arbre au dessus*)
let aux_transf_in_Untree(pile : tree list): tree list=
  match pile with
  |var1::tl_pile -> (Unary(var1)::tl_pile)
  |[] -> failwith "manque un chiffre ou une constante"
;;

(*Transforme les deux premiers arbres d'une liste d'arbre et un operateur en arbre binaire et retourne le reste de la liste avec le nouvel arbre au dessus*)
let aux_transf_in_Betree( pile , ope : tree list * operator): tree list =
  match pile with
  |cst1::cst2::tl_pile ->( Binary(ope, cst2, cst1))::tl_pile
  |[] -> failwith "manque un chiffre ou une constante"
  |_::[] -> failwith "manque un chiffre ou une constante"
;;

(*Transfert les tokens d'une liste de token dans une liste d'arbre en les convertissants en meme temps*)
let token_transf_in_tree( token, pile : token * tree list) : tree list =
  match token  with 
  |Variable(var) -> Var(var)::pile
  |Number(cst) -> Cst(cst)::pile
  |Add-> aux_transf_in_Betree(pile , Plus)
  |Subtract -> aux_transf_in_Betree(pile, Minus);
  |Minus-> aux_transf_in_Untree(pile) 
  |Multiply-> aux_transf_in_Betree(pile , Mult)  
  |Divide  -> aux_transf_in_Betree(pile , Div)
  |End -> []
;;

(*Convertie une liste de token en list d'arbre*)
let rec transf_in_tree ( tokens, pile : token list * tree list) : tree =
  match tokens with
  |[] -> List.hd (pile)
  |head::tail -> transf_in_tree(tail, token_transf_in_tree(head, pile)) 
;;

(****************************************************
                   fonctions affichages trees
 ****************************************************)
(*affiche un arbre sans le simplifier et sans réductions des parenthéses*)
let rec print_tree (tree: tree) : string = 
  match tree with
  |Var(f)       -> Char.escaped f
  |Cst(f)       -> Int.to_string f
  |Unary(t)     -> "(-" ^ ( print_tree t) ^ ")"
  |Binary(n,l,r)->
     match n with
     |Plus -> "(" ^  print_tree (l) ^ "+" ^  print_tree (r) ^ ")"
     |Minus-> "(" ^  print_tree (l) ^ "-" ^  print_tree (r) ^ ")"
     |Mult -> "(" ^  print_tree (l) ^ "*" ^  print_tree (r) ^ ")"
     |Div  -> "(" ^  print_tree (l) ^ "/" ^  print_tree (r) ^ ")"
;;

(*simplifie un arbre en réalisant les opérations simples*)
let rec simpl_tree(tree : tree) : tree =
  match tree with
  |Binary(Plus,Cst(cst1), Cst(cst2)) -> Cst(cst1+cst2)
  |Binary(Minus,Cst(cst1), Cst(cst2)) -> Cst(cst1-cst2)
  |Binary(Div,Cst(cst1), Cst(cst2)) -> Cst(cst1/cst2)
  |Binary(Mult, Cst(cst1), Cst(cst2)) -> Cst(cst1*cst2)
  |Binary(o,Cst(cst1),r) -> Binary(o,  simpl_tree(Cst(cst1)),  simpl_tree(r))
  |Binary(o,l,Cst(cst2)) -> Binary(o,  simpl_tree(l),  simpl_tree(Cst(cst2)))
  |Binary(o,l,r) -> simpl_tree(Binary(o, (l),  simpl_tree(r)))
  |Unary(_) -> tree
  |Var(_) -> tree
  |Cst(_) -> tree
;;

(*affiche un arbre simplifie et avec réductions des parenthéses*)
let rec affich_simp (tree: tree) : string =
  let rec remove_par(tree : tree) : string = (*fonction interne supprimant les parenthéses inutiles*)
    match tree with
    | Binary(Plus,Binary(o1,l1,r1),n) -> "(" ^ affich_simp(Binary(o1,l1,r1)) ^ "+" ^ affich_simp(n) ^ ")"
    | Binary(Plus,n,Binary(o1,l1,r1)) -> "(" ^ affich_simp(n) ^ "+" ^ affich_simp(Binary(o1,l1,r1)) ^ ")"
    | Binary(Minus,Binary(o1,l1,r1),n) -> "(" ^ affich_simp(Binary(o1,l1,r1)) ^ "-" ^ affich_simp(n) ^ ")"
    | Binary(Minus,n,Binary(o1,l1,r1)) -> "(" ^ affich_simp(n) ^ "-" ^ affich_simp(Binary(o1,l1,r1)) ^ ")"
    | Binary(Plus,l,r) -> affich_simp(l) ^ "+" ^ affich_simp(r)
    | Binary(Minus,l,r)-> affich_simp(l) ^ "-" ^ affich_simp(r)
    | Binary(Mult,l,r) -> affich_simp(l) ^ "*" ^ affich_simp(r)
    | Binary(Div,l,r)  -> affich_simp(l) ^ "/" ^ affich_simp(r)
    | _ -> ""
  in
  match tree with
  |Var(f)       -> Char.escaped f
  |Cst(f)       -> Int.to_string f
  |Unary(t)     -> "(-" ^ (print_tree t) ^ ")"
  |Binary(n,l,r)-> remove_par(tree)
;;


(****************************************************
                   main
 ****************************************************)
let main l  =
  let arbre : tree = transf_in_tree(l, []) in
  print_string(print_tree(arbre));
  
  let arbre_simp : tree =  simpl_tree(arbre) in
  print_string(affich_simp(arbre_simp));
;;


main(input_to_token_list());;
