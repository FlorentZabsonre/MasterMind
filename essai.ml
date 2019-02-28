type couleur =Rouge | Noir | Vert | Bleu | Jaune | Blanc | Orange | Violet ;;
(*creation du type couleur*)
type pions= { nom : couleur; n:int};;
(*creation du type pions ayant une couleur et un entier *)
let p1={nom = Rouge ; n = 1 };;
let p2={nom = Noir ; n = 2 };;
let p3={nom = Vert ; n = 3 };;
let p4={nom = Bleu ; n = 4 };;
let p5={nom = Jaune ; n = 5 };;
let p6={nom = Blanc ; n = 6 };;
let p7={nom = Orange ; n = 7 };;
let p8={nom = Violet ; n = 8 };;

let list = [ p1 ; p2; p3; p4];;

let tab_de_couleur = [|p1;p2;p3;p4;p5;p6;p7;p8|];;

let pionscouleur x = x.nom;;
let pionsInt x = string_of_int x.n;;

(*let rec trouveCouleur l n = 
	match l with
	[] -> raise (Failure "trouveCouleur")
	| x :: l -> if n <= 0 then () else pionscouleur l ; trouveCouleur l (n-1);;*)

let trouveCouleur l = 
	match l with
	[] -> raise (Failure "trouveCouleur")
	| tete :: queue -> tete :: queue;;

let listPrincipale = [];;

let insere_debut listPrincipale sousList = sousList :: listPrincipale;;

(*let sousList = [1;2;3;4;5];;*)

(*insere_debut listPrincipale sousList;;*)

let rec afficheSousListe l = 
	match l with
	| [] -> print_string "videSL"
	| tete :: queue ->  print_int tete ;afficheSousListe queue;;

let rec afficheListP l = 
	match l with
	| [] -> print_string "videLP"
	| tete :: queue ->  afficheSousListe tete ;afficheListP queue;;

#trace insere_debut;;
#trace afficheListP;;
let sousList = [1;2;3;4;7];;
insere_debut listPrincipale sousList;;
let slist=[1;2;3;4;5];;
let sl i j x y z = [i;j;x;y;z];;

let rec inserer slist listPrincipale =
	match listPrincipale with
	 | [] -> slist
	 | tete :: queue -> slist @ listPrincipale ;;

(*insere_debut listPrincipale slist;;
*)
#trace inserer;;
inserer;;

let combinaison tab_de_couleur= 
	for i = 0 to Array.length tab_de_couleur do
		for j = 0 to Array.length tab_de_couleur do
			for x = 0 to Array.length tab_de_couleur do
				for y = 0 to Array.length tab_de_couleur do
					for z = 0 to Array.length tab_de_couleur do
					(*ajouterListeDansListePossibl({tab_de_couleur[i];tab_de_couleur[j];tab_de_couleur[x];tab_de_couleur[y]})*)
						if i != j && i != x && i != y && i != z && j != x && j != y && y != z && x != y && x != z && y != z then
							(*(print_int i;print_int j;print_int x;print_int y;print_int z;print_newline ();)*)
							(*(::)([i;j;x;y;z], listPrincipale)*)
							(*[1;2;3;4;5] :: listPrincipale );*)
							(let sousList = [1;2;3;4;5] in sousList::listPrincipale;)
					done;
				done;
			done;
		done;
	done;;