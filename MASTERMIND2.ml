let reponse = ["rouge";"vert"];;

let rec construire taille = 
	let rec construirepossi list =
		match list with
			| [] -> []
			| t::q -> (("rouge"::t)::(("vert"::t)::(("bleu"::t)::(("jaune"::t)::(("blanc"::t)::(("orange"::t)::(("noir"::t)::(("violet"::t)::(construirepossi q)))))))))
	in match taille with
		|1 -> [["rouge"];["vert"];["jaune"];["bleu"];["violet"];["blanc"];["noir"];["orange"]]
		|_-> construirepossi (construire  (taille-1));;

let liste_combinaisons = construire 5;;
let question1 = "Combien de pion bien places?\n";;
let question2 = "Combien de pion mal places?\n";;

let rec afficher liste = 
	match liste with
	| [] -> ()
	| tete::queue -> (print_string tete; afficher queue;print_string "\n");;

let taille x = List.hd x;;

let rec verifcombinaison liste_combinaisons liste_en_memoire liste_reponse bp mp =
	match (liste_combinaisons,liste_en_memoire,bp,mp) with
		| ([],_,0,0) -> true
		| (tete1::queue1,tete2::queue2,bp,mp) -> if tete1=tete2 then
													verifcombinaison queue1 queue2 liste_reponse (bp-1) mp
												else if List.mem tete1 liste_reponse then
														verifcombinaison queue1 queue2 liste_reponse bp (mp-1)
													else verifcombinaison queue1 queue2 liste_reponse bp mp
		|_->false;;

let rec suppression_combinaisons proposition liste_combinaisons bp mp =
	match liste_combinaisons with
		| [] -> []
		| tete::queue -> if (verifcombinaison tete proposition proposition bp mp) then 
							tete::(suppression_combinaisons proposition queue bp mp)
						else(suppression_combinaisons proposition queue bp mp);;

let rec jouer liste_combinaisons = 
	match liste_combinaisons with
		|[] -> failwith "TRICHEUR"
		|tete::queue -> afficher tete; 
			Printf.printf "%s" question1;
			let bp = read_int () in 
			Printf.printf "%s" question2;
			let mp = read_int () in
			jouer (suppression_combinaisons (List.hd liste_combinaisons) liste_combinaisons bp mp)
			 if  ((bp = 5)  && (mp= 0)) then print_string "partie gagnée";;


let question_version = "Quelle version ? :\n";;
let  couleur= "voici les couleurs disponible rouge , noir , vert , bleu, jaune , blanc , orange , violet ",
let mode_de_jeu = 
	Printf.printf "%s" question_version;
	let x = read_int() in 
	Printf.printf "%s" couleur;
		if x = 1 then
			jouer liste_combinaisons
		else if x = 2 then 
			print_string ("Connard");;


(*let reponse = ["bleu";"blanc";"rouge";"blanc";"bleu"];;
let x = ["bleu";"blanc";"rouge";"blanc";"bleu"];;
let list = ["rouge";"vert";"bleu";"jaune";"blanc";"orange";"noir";"violet"];;

let rec construire taille = 
	let rec construirepossi list =
		match list with
			| [] -> []
			| t::q -> (("rouge"::t)::(("vert"::t)::(("bleu"::t)::(("jaune"::t)::(("blanc"::t)::(("orange"::t)::(("noir"::t)::(("violet"::t)::(construirepossi q)))))))))
	in match taille with
		|1 -> [["rouge"];["vert"];["jaune"];["bleu"];["violet"];["blanc"];["noir"];["orange"]]
		|_-> construirepossi (construire  (taille-1));;

let liste_combinaisons = construire 5;;

(*parcours simultanée des 2 liste; tete1 et queue1 pour la liste1 et tete2 et queue2 pour la listereponse*)

let rec verifcombinaison liste_combinaisons liste listreponse bp mp =
	match (liste_combinaisons,liste,bp,mp) with
		| (liste_combinaisons,listreponse,0,0) -> true
		| (tete1::queue1,tete2::queue2,bp,mp) -> if tete1=tete2 then
													verifcombinaison queue1 queue2 listreponse (bp-1) mp
												else if List.mem tete1 listreponse then
														verifcombinaison queue1 queue2 listreponse bp (mp-1)
													else verifcombinaison queue1 queue2 listreponse bp mp
		|_->false;;

let rec suppcombinaison2 prop liste bp mp =
	match liste with
		| [] -> []
		| tete::queue -> if (verifcombinaison tete prop prop bp mp = true )then 
							tete::(suppcombinaison2 prop queue bp mp)
						else(suppcombinaison2 prop queue bp mp);;
let rec afficher liste = 
	match liste with
	| [] -> ()
	| tete::queue -> (print_string tete; afficher queue);;

let rec lancer_partie liste_combinaisons = 
	match liste_combinaisons with
		| [] -> failwith "ERROR1"
		| [derniere_combinaison]-> print_string "la combinaison est \n"; afficher derniere_combinaison; print_string "\n";
        | _ -> afficher (List.hd liste_combinaisons); 
         print_string "\n"; lancer_partie (suppcombinaison2 ((List.hd liste_combinaisons) reponse 0 0))
     ;;*)