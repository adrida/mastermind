open List

(* Création d'un module avec création type couleur et les fonctions associées *)
module ListCouleur=struct 

(* Création d'un type couleur *)
type couleur= Rouge | Bleu | Vert | Noir | Jaune | Orange | Violet | Blanc

let listeCouleur= [[Rouge];[Bleu];[Vert];[Noir];[Jaune];[Orange];[Violet];[Blanc]]

(* Création de la liste de coup possible avec redondance *)
let rec construire_ListR taille listC=
  let rec construire_aux list= 
    match list with
    |[]->[]
    |h::t->(Rouge::h)::(Bleu::h)::(Vert::h)::(Noir::h)::(Jaune::h)::(Orange::h)::(Violet::h)::(Blanc::h)::(construire_aux t)
  in match taille with 
  |1 -> listC
  |_-> if taille>=1 then
      construire_aux (construire_ListR (taille-1) listC)
    else 
      failwith "Erreur taille trop petite"

(* Création dela fonction de comparaison *)
let rec compList list =
  match list with
  | []->true
  |h::t-> not(mem h t) && compList t 

(* Création de la liste sans redondance de couleur *)
let rec  construire_ListSR list comp =
  let rec aux l res=
    match l with 
  |[]->res
  |h::t->if not(comp h) then
      aux t res
    else
      aux t (h::res)
  in aux list []

(* Supprime la 1ere occurence de e dans l *)

let suppression e l =
  let rec aux res l2 = match l2 with
      [] -> res
    | h::t -> if e = h then res @ t else aux (res @ [h]) t
  in aux [] l ;;

(* Compare deux combinaisons et retourne le nombre de pions communs *)
let pions_communs l1 l2 =
  let rec aux l1 l2 = match l1 with
      [] -> l2
    | h::t -> aux t (suppression h l2)
  in (List.length l2) - (List.length (aux l1 l2));;

(* Compare deux combinaisons et retourne le nombre de pions bien places *)
let pions_bien_places l1 l2 =
  let rec aux n l1 l2 = match l1,l2 with
    | [],_ -> n
    |_,[]->n
    | h1::t1,h2::t2 -> if h1 = h2 then aux (n + 1) t1 t2 else aux n t1 t2
  in aux 0 l1 l2 ;;

(* Compare deux combinaisons et retourne un couple d'entiers (bp, mp) *)
(* bp est le nombre de pions bien places et mp le nombre de pions mal places *)
let indications l1 l2 =
  let bp = pions_bien_places l1 l2 in
  let mp = (pions_communs l1 l2) - bp in (bp, mp);;

(* comb est la combinaison proposee par l'ordinateur *)
(* ind est le couple d'indications donnee par l'utilisateur *)
(* l est la liste des solutions potentielles selon l'ordinateur *)
(* Supprime de l toutes les combinaisons qui ne peuvent etre la solution *)
let elagage comb ind l =
  let rec aux l1 l2 = match l2 with
      [] -> l1
    | h::t -> if (indications comb h) = ind
        then aux (l1 @ [h]) t
        else aux l1 t
  in aux [] l ;;
(*
let code_of_string_bis s =
    let ls = (Str.split (Str.regexp " ") s ) in
        let rec aux acc ls = match ls with
            |[]-> acc
            |h::t -> aux ((couleur(h)::acc)) t in aux [] ls;; 

let code_of_string s couleur_possible = if (((code_of_string_bis s)<>[] ) && (contient couleur_possible (code_of_string_bis s)))
                then Some(code_of_string_bis s)
            else None;;

let string_of_code code = 
    let rec aux acc = function
        |[] -> acc
        |Couleur(s)::tail -> aux (s^" "^acc) tail in aux "" (code:t);;


let rec saisie couleur_possible tailleCode = print_string("Entrer votre code : "); 
        let s=read_line () in let codeEntre=(code_of_string s couleur_possible) in 
            if ((codeEntre)=None) then (print_string("Saisie incorrecte (Tout écrire en minuscule ou couleurs non définies) : "); saisie couleur_possible tailleCode)
                else if ((List.length (code_of_string_bis s))>tailleCode)  then (print_string("Saisie incorrecte (Code trop grand) : "); saisie couleur_possible tailleCode)
                     else if ((List.length (code_of_string_bis s))<tailleCode) then (print_string("Saisie incorrecte (Code trop petit) : "); saisie couleur_possible tailleCode)
                    else List.rev (code_of_string_bis s);;

 Affichage des couleurs  *)
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

end ;;