open Expression_scanner;;

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
  |cst1::cst2::tl_pile ->( Binary(ope, cst2, cst1))::tl_pile
  |[] -> failwith "manque un chiffre ou une constante"
  |_::[] -> failwith "manque un chiffre ou une constante"
;;

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

let rec transf_in_tree ( tokens, pile : token list * tree list) : tree =
  match tokens with
  |[] -> List.hd (pile)
  |head::tail -> transf_in_tree(tail, token_transf_in_tree(head, pile)) 
;;

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

let rec affich_simp (tree: tree) : string =
  let rec remove_par(tree : tree) : string =
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

let main l  =
  let arbre : tree = transf_in_tree(l, []) in
  print_string(print_tree(arbre));
  
  let arbre_simp : tree =  simpl_tree(arbre) in
  print_string(affich_simp(arbre_simp));
;;


main(input_to_token_list());;
