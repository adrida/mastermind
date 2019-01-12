open List;;
open ListCouleur;;

exception Perdu;;

	(* fonction de jeu: affichage de la propostion *)
	(* elagage de la liste en fonction de bp et mp *)
(* test de Perdu et combinaison caché *)

	let jouer_true nbcoup liste listerep listeRAN= 
	let rec aux n l =
	match (n,l) with
	|_,[]-> raise Perdu
	|x,listeRAN->
	let prop =hd l in
	ListCouleur.print_list prop;
	print_string "est la bonne combinaison !\n";
	print_newline();
		Sys.command "clear";

	|x,_ when x>nbcoup && nbcoup<>0->
	print_string "Plus d'essai: PERDU!\n";
	print_newline();
		Sys.command "clear";

	|x,_->
	let prop=hd l in
	print_string "Essai ";
	print_int x;
	print_string " : ";
	ListCouleur.print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let bp= fst (indications listerep liste) in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp= snd (indications listerep liste) in
	print_newline();
	aux (x + 1) (ListCouleur.elagage prop (bp,mp) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;;


	let jouer_false nbcoup liste listerep =
	let rec aux n l =
	match (n,l) with
	|_,[]-> raise Perdu
	|x,[h]->
	let prop =hd l in
	ListCouleur.print_list prop;
	print_string "est la bonne combinaison !\n";
	print_newline();
		Sys.command "clear";

	|x,_ when x>nbcoup && nbcoup<>0->
	print_string "Plus d'essai: PERDU!\n";
	print_newline();
		Sys.command "clear";

	|x,_->
	let prop=hd l in
	print_string "Essai ";
	print_int x;
	print_string " : ";
	ListCouleur.print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let bp=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp=read_int() in
	print_newline();
aux (x + 1) (ListCouleur.elagage prop (bp,mp) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;;


	(* Menu récursif pour rejouer *)
	(* gestion de la Perte*)

	let rec menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN= 
	print_newline();
	print_string ">>  Joueur :  ";
	print_string nom_joueur;
	print_string "  << \n";
	print_string "Veuillez saisir votre combinaison  : \n";
	print_string "pour la beta voici la lite generee randomisee  : \n";
if auto == false then 
	try
	print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	print_int coup_max;
	print_string " coups  || \n\n";
	print_string "	Créez votre code et retenez le bien !\n Appuyez sur \n -> 1 pour jouer sans redondance ou \n -> 2 pour jouer avec.\n\n\n  --->>>> Pour quitter tappez 0 ...\n";
	let mode=read_int() in
	match mode with
	|2-> jouer_false coup_max listeSR listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	|1->jouer_false coup_max listeComplete listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	|0->print_string "Merci d'avoir joué!\n"
	|_-> print_string "Erreur de choix\n";
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; 
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	|Perdu -> print_string "Perdu! C'est pas bien\n";
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN;
else 
	 try
	 print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	 print_int coup_max;
	 print_string " coups  || \n";
	 print_string "Vous avez choisi le mode 'true' nous allons donc générer pour vous une combinaison ! \n \n";
	 print_string " ... \n";
	 print_string " ... \n\n\n";
	 print_string " >> SUCCES, Voici votre combinaison :  \n\n";
	 ListCouleur.print_list (listeRAN);
	 print_string " \n\n\n\n";
	 print_string "Appuyez sur \n -> 1 pour jouer sans redondance ou \n -> 2 pour jouer avec.\n\n\n  --->>>> Pour quitter tappez 0 ...\n";
	 let mode=read_int() in
	 match mode with
	 |1-> jouer_true coup_max listeSR listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	 |2->jouer_true coup_max listeSR listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	 |0->print_string "Merci d'avoir joué!\n"
	 |_-> print_string "Erreur de choix\n";
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; 		menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  


	|Perdu -> print_string "Perdu! C'est pas bien\n";		menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  

;;
	


(* Construction des listes de combinaison support*)
	let listeComplete= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
	let listeSR= ListCouleur.construire_ListSR listeComplete ListCouleur.compList;;
	let listeComplete1recuperee= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
	let list_intermediaire= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
    let listeRAN = List.nth (list_intermediaire) (Random.int (10000));;

    (* Fonction de lancement principale *)

	let mastermind nom_joueur coup_max nb_parties auto =
	Sys.command "clear";
	print_string "MASTERMIND \n";
	print_string "=====BIENVENUE A TOI ";
	print_string nom_joueur;
	print_string "   =====\n\n\n\n";
	menu2 nom_joueur coup_max nb_parties auto listeComplete listeSR listeComplete1recuperee listeRAN;
	Sys.command "clear";;

let name = "Player";;
	mastermind (name) 5 1 (true);;