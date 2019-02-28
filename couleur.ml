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

let rand()=
Random.self_init();
let  de = (Random.int 8) + 1  in
 print_int de
  ;;

let x=rand();;

let tableau_point= [|p1;p2;p3;p4;p5;p6;p7;p8|];
  
let listpo() =
   let l =[];
 for i = 0 to vect_length tableau_point - 1 do
   d=tableau_point(i);
   let rand()=
     Random.self_init();
     let  de = (Random.int 8) + 1;
       if de == d.n  then 
	 let rec ajoutpoint d l = match l with
	   | [] -> d::[]
	   |tete:: queue -> tete::ajoutpoint d l
	   |l -> d::l;
 done;;
#trace listpo();;
listpo();;
