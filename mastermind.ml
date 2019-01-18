open List;;
open Tools;;

exception Triche;;
exception Start;;

	(**fonction : proposition *)
	(** fonction de jeu: affichage de la propostion *)
(** resolution de la liste en fonction de pl_corr et pl_mauvais *)

(**-> COMPARER listeRAN et La liste que lordi teste *) 
	let jouer_true nbcoup liste listerep= 
	let rec aux n l =
	match (n,l) with
	|_,[]-> raise Triche
	|x,listeRAN->
	let prop =hd l in
	Tools.print_list prop;
	print_string "est la bonne combinaison !\n";
	print_newline();

	|x,_ when x > nbcoup && nbcoup <> 0->
	print_string "Plus d'essai: Triche!\n";
	print_newline();

	|x,_->
	let prop=hd l in
	print_string "Essai ";
	print_int x;
	print_string " : ";
	Tools.print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let pl_corr= fst (ft_data_game listerep liste) in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let pl_mauvais= snd (ft_data_game listerep liste) in
	print_newline();
aux (x + 1) (Tools.resolution prop (pl_corr,pl_mauvais) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;;


	let jouer_false nbcoup liste listerep =
	let rec aux n l =
	match (n,l) with
	|_,[]-> raise Triche
	|x,[h]->
	let prop =hd l in
	Tools.print_list prop;
	print_string "est la bonne combinaison !\n";
	print_newline();
	print_string "===================================================\n";
	print_string "===================================================\n";

	|x,_ when x>nbcoup && nbcoup<>0->
	print_string "Plus d'essai: Triche!\n";
	print_newline();
	print_string "===================================================\n";
	print_string "===================================================\n";

	|x,_->
	let prop=hd l in
	print_string "Essai ";
	print_int x;
	print_string " : ";
	Tools.print_list prop;
	print_string "\n";
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let pl_corr=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let pl_mauvais=read_int() in
	print_newline();
aux (x + 1) (Tools.resolution prop (pl_corr,pl_mauvais) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;;


	(** Menu récursif pour rejouer *)
(** gestion de la Perte*)

	let rec menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN= 
	print_newline();
	print_string ">>  Joueur :  ";
	print_string nom_joueur;
	print_string "  << \n\n";
	print_string "     /!\\  Normalement a ce moment l'utilisateur saisit la combinaison /!\\ \n\n";
	print_string "Creez une combinaison et retenez la bien    !!!!! \n\n";

	if auto == false then 
	try
	print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	print_int coup_max;
	print_string " coups  || \n\n";
	print_string "	Créez votre code et retenez le bien !\n Appuyez sur \n -> 1 pour jouer sans redondance ou \n -> 2 pour jouer avec.\n\n\n  --->>>> Pour quitter tappez 0 ...\n";
	print_string "===================================================\n";
	print_string "===================================================\n";
	let mode=read_int() in
	match mode with
	|2-> jouer_false coup_max listeSR listeRP;
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	|1->jouer_false coup_max listeComplete listeRP;
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	|0->print_string "Merci d'avoir joué!\n"
	|_-> print_string "Erreur de choix\n";
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; 
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN
	|Triche -> print_string "Triche! C'est pas bien\n";
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN;
	else 
	try
	print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	print_int coup_max;
	print_string " coups  || \n";
	print_string "Vous avez choisi le mode 'true' nous allons donc générer pour vous une combinaison ! \n \n";
	print_string " ... \n";
	print_string " ... \n\n\n";
	print_string " >> SUCCES, Voici votre combinaison :  \n\n";
	Tools.print_list (listeRAN);
	print_string " \n\n\n\n";
	print_string "Appuyez sur \n -> 1 pour jouer sans redondance ou \n -> 2 pour jouer avec.\n\n\n  --->>>> Pour quitter tappez 0 ...\n";
	print_string "===================================================\n";
	print_string "===================================================\n";
	let mode=read_int() in
	match mode with
	|1-> jouer_true coup_max listeSR listeRP;
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	|2->jouer_true coup_max listeSR listeRP;
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	|0->print_string "Merci d'avoir joué!\n"
	|_-> print_string "Erreur de choix\n";
	menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; 		menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  


	|Triche -> print_string "Triche! C'est pas bien\n";		menu nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP listeRAN  

	;;



(** Construction des listes de combinaison support et exception Start pour forcer le programmme a compiler*)
	let listeComplete= Tools.avec_redondance 5 Tools.listePion;;
	let listeSR= Tools.sans_redondance listeComplete Tools.compList;;
	let listeComplete1recuperee= Tools.avec_redondance 5 Tools.listePion;;
	let list_intermediaire= Tools.sans_redondance listeComplete Tools.compList;;
	let listeRAN = List.nth (list_intermediaire) (1);;


	(** Fonction de lancement principale
	 * @param nom du joueur, le nb de coups et de parties, true : sans intervention du joueur et false : avec intervention
	 * @return le jeux 
	 *)


	let mastermind nom_joueur coup_max nb_parties auto =
	Sys.command "clear";
	print_string "MASTERMIND \n";
	print_string "=====   BIENVENUE A VOUS ";
	print_string nom_joueur;
	print_string "   =====\n\n\n\n";
	menu nom_joueur coup_max nb_parties auto listeComplete listeSR listeComplete1recuperee listeRAN;
	Sys.command "clear";;

	(** commandes pour plus de claretee dans l'affichage
	 *)
	Sys.command "clear";;
	print_string "\n\n /!\\ >> Pour lancer une partie appeler sur la console ocaml la fonction mastermind de cette maniere :  \n\n
	>> # mastermind nom_joueur coup_max nb_parties auto \n\n
	nom_joueur : Votre nom ;) \n
	coup_max : Le nombre de coups maximum pour deviner le code \n
	nb_parties : Le nombre de parties souhaitees (PAS IMPLEMENTE POUR L'INSTANT) \n
	auto : true ou false - permet de preciser si vous voulez que les reponses soient calculees de maniere automatiques ou non \n
	---> ATTENTION SEUL LE MODE FALSE FONCTIONNE \n\n";;




