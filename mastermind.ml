open List;;
open ListCouleur;;

exception Perdu;;

	(* fonction de jeu: affichage de la propostion *)
	(* elagage de la liste en fonction de bp et mp *)
(* test de Perdu et combinaison caché *)

	let jouer_true nbcoup liste listerep = 
	let rec aux n l =
	match (n,l) with
	|_,[]-> raise Perdu
	|x,[h]->
	let prop =hd l in
	ListCouleur.print_list prop;
	print_string "est la bonne combinaison !\n";
	print_newline();
	|x,_ when x>nbcoup && nbcoup<>0->
	print_string "Plus d'essai: PERDU!\n";
	print_newline();
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
	|x,_ when x>nbcoup && nbcoup<>0->
	print_string "Plus d'essai: PERDU!\n";
	print_newline();
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

	(*
	 let jouer  nbcoup liste auto listerep =
	 let rec aux n l =
	 match (n,l) with
	 |_,[]-> raise Perdu
	 |x,[h]->
	 let prop =hd l in
	 ListCouleur.print_list prop;
	 print_string "est la bonne combinaison !\n";
	 print_newline();
	 |x,_ when x>nbcoup && nbcoup<>0->
	 print_string "Plus d'essai: PERDU!\n";
	 print_newline();
	 |x,_->
	 let prop=hd l in
	 print_string "Essai ";
	 print_int x;
	 print_string " : ";
	 ListCouleur.print_list prop;
	 print_string "\n";
	 if auto == false then
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let bp=read_int() in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp=read_int() in
	print_newline();
aux (x + 1) (ListCouleur.elagage prop (bp,mp) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;
	else 
	print_string "Nombre de pion(s) bien placé(s):\n";
	try
	let bp= fst (indication listerep liste) in
	print_string "Nombre de pions(s) mal placé(s):\n";
	let mp= snd (indication listerep liste) in
	print_newline();
aux (x + 1) (ListCouleur.elagage prop (bp,mp) l)
	with Failure("int_of_string") ->print_string "Erreur de saisie!\n"; aux x l;
	in aux 1 liste;;
	*)
	(* Menu récursif pour rejouer *)
	(* gestion de la Perte*)
(* choix du mode de jeu *)
	(*
	 let rec menu2 listC listSR=
	 print_newline();
	 print_string "Choisissez votre combinaison de 5 couleurs parmis les couleurs suivantes and keep it in your mind :):\n -Rouge|Bleu|Vert|Noir|Jaune|Orange|Violet|Blanc-\n";
	 try
	 print_string "Choisissez votre mode de jeu (1: Sans redondance, 2: Avec redondance, 3: avec nombre de coup limité, 0: quitter)\n";
	 let mode=read_int() in
	 match  mode with
	 |1->jouer 0 listSR auto ;
	 menu listC listSR    
	 |2->jouer 0 listC auto ;
	 menu listC listSR   
	 |3->print_string "Combien de coups maximum souhaitez vous?\n";
	 let nbcoup=read_int() in        
	 jouer nbcoup listC auto ;
	 menu listC listSR      
	 |0->print_string "Merci d'avoir joué!\n"
	 |_-> print_string "Erreur de choix\n";
	 menu listC listSR 
	 with 
	 |Failure("int_of_string")-> print_string "Erreur de saisie!\n"; menu listC listSR
	|Perdu -> print_string "Perdu! C'est pas bien\n"; menu listC listSR;;
	*)
	let rec menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP= 
	print_newline();
	print_string ">>  Joueur :  ";
	print_string nom_joueur;
	print_string "  << \n";
	print_string "Veuillez saisir votre combinaison  : \n";
	print_string "pour la beta voici la lite generee randomisee  : \n";
(*A CHANGER*)
(*if auto == false then *)
	try
	print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	print_int coup_max;
	print_string " coups  || \n";
	print_string "Créez votre code et retenez le bien ! Appuyez sur 1 pour jouer sans redondance ou 2 pour jouer avec.\n  --->>>> Pour quitter tappez 0 ...\n";
	let mode=read_int() in
	match mode with
	|2-> jouer_false coup_max listeSR listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP  
	|1->jouer_false coup_max listeComplete listeRP;
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP
	|0->print_string "Merci d'avoir joué!\n"
	|_-> print_string "Erreur de choix\n";
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; 
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP
	|Perdu -> print_string "Perdu! C'est pas bien\n";
	menu2 nom_joueur coup_max nb_parties auto listeSR listeComplete listeRP;; 
	(* else 
	 try
	 print_string "Parfait nous pouvons commencer : Le nombre de coups maximum est de : ||   ";
	 print_string coup_max;
	 print_string " coups  || \n";
	 print_string "Vous avez choisi le mode 'true' nous allons donc générer pour vous une combinaison ! \n \n"
	 print_string " ... \n";
	 print_string " ... \n\n";
	 print_string " >> SUCCES, Voici votre combinaison :  \n";
	 print_list listeComplete;
	 print_string "Appuyez sur 1 pour jouer sans redondance ou 2 pour jouer avec.\n  --->>>> Pour quitter tappez 0 ...\n";
	 let mode=read_int() in
	 match mode with
	 |1-> jouer_true coup_max listSR listerep;
	 menu2 nom_joueur coup_max nb_parties auto   
	 |2->jouer_true coup_max listC listerep;
	 menu2 nom_joueur coup_max nb_parties auto 
	 |0->print_string "Merci d'avoir joué!\n"
	 |_-> print_string "Erreur de choix\n";
	 menu2 nom_joueur coup_max nb_parties auto
	with 
	|Failure("int_of_string")-> print_string "Erreur de saisie!\n"; menu2 nom_joueur coup_max nb_parties auto
	|Perdu -> print_string "Perdu! C'est pas bien\n";menu2 nom_joueur coup_max nb_parties auto;;
	*)


(* Construction des listes de combinaison*)
	let listeComplete= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;
	let listeSR= ListCouleur.construire_ListSR listeComplete ListCouleur.compList;;
	let listeComplete1= ListCouleur.construire_ListR 5 ListCouleur.listeCouleur;;

	(*
	 let main(nom_joueur) =
	 print_string "        Mastermind\n\n";
	 print_string "=====   BIENVENUE A TOI ";
	 print_string nom_joueur;
	 print_string "   =====\n";
	 menu listeComplete listeSR;
	 print_string "Au revoir! \n";;

	 *)

	let mastermind nom_joueur coup_max nb_parties auto =
	Sys.command "clear";
	print_string "MASTERMIND \n";
	print_string "=====BIENVENUE A TOI ";
	print_string nom_joueur;
	print_string "   =====\n";
	menu2 nom_joueur coup_max nb_parties auto listeComplete listeSR listeComplete1;
	Sys.command "clear";;

	(*main("Hassan");;*)
let name = "hassan";;
	mastermind (name) 5 1 (false);;