open List

(** Création d'un module avec création type Pion et les fonctions associées *)
module Tools=struct 

(** Création d'un type Pion *)
type pion= Rouge | Bleu | Vert | Noir | Jaune | Orange | Violet | Blanc

let listePion= [[Rouge];[Bleu];[Vert];[Noir];[Jaune];[Orange];[Violet];[Blanc]]

(** Création de la liste de coup possible avec redondance
* @param taille de la liste desirer et la liste complete
* @return genere une liste de coup possible avec redondance
*)
	let rec avec_redondance taille listC=
	let rec construire_aux list= 
	match list with
	|[]->[]
|h::t->(Rouge::h)::(Bleu::h)::(Vert::h)::(Noir::h)::(Jaune::h)::(Orange::h)::(Violet::h)::(Blanc::h)::(construire_aux t)
	in match taille with 
	|1 -> listC
	|_-> if taille>=1 then
construire_aux (avec_redondance (taille-1) listC)
	else 
	failwith "Erreur de taille"

(** Création de la fonction de comparaison
* @param une liste de couleur
* @return true/false si une couleur se repete ou pas dans la liste
*)
	let rec compList list =
	match list with
	| []->true
	|h::t-> not(mem h t) && compList t 

(** Création de la liste sans redondance de Pion
* @param list
* @return une liste sans repetition de couleurs

*)
	let rec  sans_redondance list comp =
	let rec aux l res=
	match l with 
	|[]->res
	|h::t->if not(comp h) then
	aux t res
	else
aux t (h::res)
	in aux list []
(** Supprime la 1ere occurence de e dans l
* @param une couleur et une liste
* @return l sans la presence de l'element e
*)

	let ft_list_del e l =
	let rec aux res l2 = match l2 with
	[] -> res
	| h::t -> if e = h then res @ t else aux (res @ [h]) t
	in aux [] l ;;

(** Compare deux combinaisons et retourne le nombre de Pions bien places
* @param deux combinaisons
* @return le nb de Pions bien places
*)
	let ft_get_pl_corr l1 l2 =let rec aux n l1 l2 = match l1,l2 with
	| [],_->n
	|_,[]->n
	| h1::t1,h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2 in aux 0 l1 l2 ;;

(** Compare deux combinaisons et retourne le nombre de Pions communs
* @param deux listes
* @return le nb de pions communs
*)
	let ft_common l1 l2 =
	let rec aux l1 l2 = match l1 with
	[] -> l2
| h::t -> aux t (ft_list_del h l2)
	in (List.length l2) - (List.length (aux l1 l2));;


(** Compare deux combinaisons et retourne un couple d'entiers (pl_corr, pl_mauvais)
* @param deux listes
* @return un couple d'entiers qui designent pions bien placé/mal placé
*)
(** pl_corr est le nombre de Pions bien places et pl_mauvais le nombre de Pions mal places *)
	let ft_data_game l1 l2 = let pl_corr = ft_get_pl_corr l1 l2 in let pl_mauvais = (ft_common l1 l2) - pl_corr in (pl_corr, pl_mauvais);;

	(** comb est la combinaison proposee par l'ordinateur *)
	(** ind est le couple de donnees correspondants au pions bien et mal places par l'utilisateur *)
	(** l est la liste des solutions potentielles selon l'ordinateur *)
(** Supprime de l toutes les combinaisons qui ne peuvent etre la solution *)
	let resolution comb ind l =
	let rec aux l1 l2 = match l2 with
	[] -> l1
	| h::t -> if (ft_data_game comb h) = ind
	then aux (l1 @ [h]) t
	else aux l1 t
	in aux [] l ;;


(** Affichage des Pions
* @param une liste
* @return affiche la liste
*)
	let rec print_list l =
	match l with
| [] -> ()
	| h::t -> match h with
	|Rouge-> print_string "Rouge ";
	print_list t
	|Bleu -> print_string "Bleu ";
	print_list t
	|Vert->  print_string "Vert ";
	print_list t
	|Noir->  print_string "Noir ";
	print_list t
	|Jaune->  print_string "Jaune ";
	print_list t
	|Orange->  print_string "Orange ";
	print_list t
	|Violet->  print_string "Violet ";
	print_list t
	|Blanc->  print_string "Blanc ";
	print_list t ;;
  
(*

let string_of_code c = 
  let rec stoc l =
  match l with
  | [] -> ()
  | h::t -> match h with
  |Rouge->  "Rouge ";
  |Bleu ->  "Bleu ";
  |Vert-> "Vert ";
  |Noir-> "Noir ";
  |Jaune->  "Jaune ";
  |Orange-> "Orange ";
  |Violet-> "Violet ";
  |Blanc-> "Blanc ";

	let code_of_string liste_Pion =
	let rec liste_Pion_to_string_aux liste_Pion s=
	match liste_Pion with
	|[] -> "" 
  |h::t -> (string_of_code h)^(liste_Pion_to_string_aux t s)
	in liste_Pion_to_string_aux liste_Pion "";;*)
	end ;;
