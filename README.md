# prog_fonctProjet de Programmation fonctionnelle 2021:


Mise en route :




Description des fonctions :


let rec transf_in_tree ( tokens, pile : token list * tree list) : tree = 

La fonction transf_in_tree est une fonction récurrente qui prend en paramètre une liste de token ( celle saisi dans le fichier .txt) et une tree list vide pour faire notre pile d'appel.
Tous d'abord on lit le premier token puis de façon récurrente , l'envoie à la fonction token_transf_in_tree fonction 
s'occupant de transformer le token avant tous filtré par un match with, en arbre et l'ajoute dans la pile d'appel . Une fois la liste de tokens vide , on lit la seul valeur restante dans la pile d'appel notre arbre.


let rec print_tree (tree: tree) : string =

Une fonction récurrente, lis l'abre par parcours infixe et renvoie l'expression mathématique pas simplifié.

let rec transf_in_tree ( tokens, pile : token list * tree list) : tree =

Fonction récursive prenant un arbre le parcour traite les différentes possibilité par un match with , puis simplifie .
Par exemple les cas suivants :
0*x = 0, 1 + 3 = 4 , 2/2 = 1 et bien d'autre.
A la fin la fonction renvoie un arbre simplifié.

let rec affich_simp (tree: tree) : string =

Même fonctionnement que la fonction print_tree sauf qu'on simplifie en enlevant des paranthèse inutile.


Let main l =

Fonction final regroupant tous celle cité ci-desssus pour faire le cheminement suivant :
	-Lis l'expression polonaise insversé inscrit dans le fichier txt
	-Redirection sur la fonction imput_to_token_list() envoie une liste de token
	-Cette liste est analysé pour devenir un arbre
	-On affiche l'expression mathématique, contenu dans l'arbre
	-On Simplifie l'arbre 
	-On donne l'expression simplifie 

Fait par LUNETEAU Thomas et VIALLE Charlie. 

