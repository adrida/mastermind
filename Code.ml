(** Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
(** Le type d'un pion *)
type pion = (* A COMPLETER *)
(** Le type d'un code *)
type t = pion list
(** Nombre de pions par code *)
val nombre_pions : int
(** Liste des couleurs possibles *)
val couleurs_possibles : pion list
(** Compare deux codes
 let rec cmp l ll = 
match (l,ll) with
| [], [] -> 0
| [],_ -> -1
| _,[] -> 1
| (h::t), (hh::tt) -> if h > hh then 1
                      else if h < hh then -1 
                      else cmp t tt;;
*)
val compare : t -> t -> int
(** Conversion code vers chaine de caracteres (pour affichage)
* @param code code a convertir
* @return la representation en chaine de caracteres de [code]
*)
val string_of_code : t -> string
let couleur_to_string c = 
  match c with 
  |Bleu -> "Bleu;"
  |Blanc -> "Blanc;"
  |Rouge ->"Rouge;"
  |Vert->"Vert "
  |Noir->"Noir;"
  |Violet->"Violet;"
  |Jaune->"Jaune;"
  |Orange->"Orange;";;
*)
val code_of_string : string -> t option


(** La liste de tous les codes permis *)
val tous : t list 

let rec  construire taille =
  let rec construire_aux liste =
    match liste with
    |[]->[]
    |h::t -> ((Rouge::h)::((Vert::h)::((Blanc::h)::((Noir::h)::((Bleu::h)::((Jaune::h)::((Violet::h)::((Orange::h)::(construire_aux t)))))))))
  in match taille with
  |1 ->[[Rouge];[Vert];[Blanc];[Noir];[Bleu];[Jaune];[Violet];[Orange]]
  |_->construire_aux (construire(taille-1));;
  
  
(** La liste de toutes les reponses possibles *)
val toutes_reponses : (int * int) list ;;
(** Calcule la reponse d'un code par rapport au code cache
* @param code le code propose
* @param vrai_code le code cache
* @return un couple (nombre de pions bien places, nombre de pions mal places)
[None] si la reponse ne peut etre calculee
*)
val reponse : t -> t -> (int * int) option
end ;;