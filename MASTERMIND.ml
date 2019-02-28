(*****Fonctions qui font un affichage dans le programme*****)

let logo ="
           ______________________________________________
          |                                              |	   
          |                                              |
          |                  MASTERMIND                  |
          |                                              |
          |        par: - Zabsonre Ahmonkou              |
          |             - Bovie Pierre-Edouard           |
          |                                              |
          |______________________________________________|\n\n\n";;

let regles = " Les regles :
	A vous de choisir dans votre tete,
	une combinaison de 5 couleurs parmi celle-ci:
		[ rouge, jaune, blanc, bleu, violet, vert, orange, fuschia ]
	L'ordinateur vous proposera des combinaisons,
		vous lui donnerez les reponses qu'il attend.

	ATTENTION!
	Si vous vous trompez dans vos indications il risque de mal le prendre ;)\n\n";;

let question_versions = "Avec quelle version souhaiteriez-vous jouer? :\n
 	1. Premiere version (tous les pions sont de couleurs differentes)
 	2. Deuxieme version (les pions peuvent etre de meme couleur)
 	3. Troisieme version (le nombre d'essais de l'ordimateur est limite a 8)\n";;

let question1 = "Combien de pion bien places?\n";;
let question2 = "Combien de pion mal places?\n";;
let taille x = List.length x;;
let length x = Printf.printf "Taille de la liste %d\n" (taille x);;

let rec afficher_combinaison combinaison =
	match combinaison with
	|[] -> ()
	| t::q -> print_string t ; print_string " " ; afficher_combinaison q;;

let rec afficher liste_combinaisons = 
	(*Printf.printf "[ ";*)
	match liste_combinaisons with
	| [] -> ()
	| tete::queue -> (afficher_combinaison(*print_string*) tete; afficher queue; print_string "\n");;

let annonce_victoire = "Et l'ordinateur a gagne!\n";;



(*****Fonctions maitresses du programme*****)

let rec construire taille = 
	let rec construirepossi list =
		match list with
			| [] -> []
			| t::q -> (("rouge"::t)::(("jaune"::t)::(("blanc"::t)::(("bleu"::t)::(("violet"::t)::(("vert"::t)::(("orange"::t)::(("fuchsia"::t)::(construirepossi q)))))))))
	in match taille with
		|1 -> [["rouge"];["jaune"];["blanc"];["bleu"];["violet"];["vert"];["orange"];["fuchsia"]]
		|_-> construirepossi (construire  (taille-1));;

let liste_combinaisons = construire 5;;

let rec verification liste_combinaisons liste_en_memoire liste_reponse bp mp =
	match (liste_combinaisons,liste_en_memoire,bp,mp) with
		| ([],_,0,0) -> true
		| (tete1::queue1,tete2::queue2,bp,mp) -> if tete1=tete2 then
													verification queue1 queue2 liste_reponse (bp-1) mp
												else if List.mem tete1 liste_reponse then
														verification queue1 queue2 liste_reponse bp (mp-1)
													else verification queue1 queue2 liste_reponse bp mp
		|_->false;;

let rec suppression_combinaisons proposition liste_combinaisons bp mp =
	match liste_combinaisons with
		| [] -> []
		| tete::queue -> if (verification tete proposition proposition bp mp) then 
							tete::(suppression_combinaisons proposition queue bp mp)
						else(suppression_combinaisons proposition queue bp mp);;

let rec jouer liste_combinaisons nombre_essai= 
	match (liste_combinaisons,nombre_essai) with
	| ([tete],_) -> if taille liste_combinaisons = 1 then 
		print_string "L'ordinateur dit que c'est la combinaison: ";
		afficher_combinaison tete;
		Printf.printf "\n%s" annonce_victoire
		| (_,0) -> failwith "Nombre d'essai depasse par l'ordinateur.\nVous avez gagne!"
		| ([],_) -> failwith "TRICHEUR"
		| (tete::queue,_) -> print_string "L'ordinateur vous propose la combinaison : [ ";
		afficher_combinaison tete;
			print_string "]\n";
			length liste_combinaisons;		(*Ligne pour afficher le nombre de combinaisons restantes*)		
			Printf.printf "%s" question1;
			let bp = read_int () in
			Printf.printf "%s" question2;
			let mp = read_int () in
			print_string "\n";
			jouer (suppression_combinaisons (List.hd liste_combinaisons) liste_combinaisons bp mp ) (nombre_essai-1)
			;;

let rec verification_redondances_combinaison combinaison = 
	match combinaison with
	| [] -> false
	| tete::queue -> if (List.mem tete queue) then true
					else (verification_redondances_combinaison queue);;

let rec suppression_redondances_combinaison liste_combinaisons =
	match liste_combinaisons with
		| [] -> []
		| tete::queue -> if (verification_redondances_combinaison tete = false) then 
							tete::(suppression_redondances_combinaison queue)
						else (suppression_redondances_combinaison queue);;

let mode_de_jeu = 
	let x = ref 4
	in while !x <> 1 && !x <> 2 && !x <> 3 do
	Printf.printf "%s \n %s \n %s" logo regles question_versions;
		let y =read_int() in 
		 x := y;
		if y = 1 then
			jouer (suppression_redondances_combinaison liste_combinaisons) (-1)
		else if y = 2 then 
			jouer liste_combinaisons (-1)
		else if y = 3 then
			jouer liste_combinaisons 8
	done;;