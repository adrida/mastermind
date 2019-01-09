(** Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
(** Le type d'un pion *)
type pion = Rouge | Bleu | Vert | Noir | Jaune | Orange | Violet | Blanc
(** Le type d'un code *)
type t = pion [[Rouge];[Bleu];[Vert];[Noir];[Jaune];[Orange];[Violet];[Blanc]]
(** Nombre de pions par code *)
val nombre_pions : int
(** Liste des couleurs possibles *)

let rec couleurs_possibles listC=
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

val compare : t -> t -> int
(** Conversion code vers chaine de caracteres (pour affichage)
* @param code code a convertir
* @return la representation en chaine de caracteres de [code]
*)
val string_of_code : t -> string
(** Conversion chaine de caracteres vers code (pour saisie)
* @param string chaine de caractere saisie
* @return le code correspondant a la saisie si la conversion est possible
[None] si la conversion n'est pas possible
*)
val code_of_string : string -> t option

(** La liste de tous les codes permis *)
val tous : t list

(** La liste de toutes les reponses possibles *)
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
let toutes_reponses l1 l2 =
  let bp = pions_bien_places l1 l2 in
  let mp = (pions_communs l1 l2) - bp in (bp, mp);;
  
val reponse : t -> t -> (int * int) option
end ;;