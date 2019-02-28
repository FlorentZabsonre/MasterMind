open Printf;;
(*let rec filtrer combinaison proposition_aux proposition (bp,mp) = 
	match (combinaison,proposition_aux,bp,mp) with
	| ([],_,5,5) -> true
	| (couleur1::q1,couleur2::q2,bp,mp) ->	if couleur1=couleur2 then 
												filtrer q1 q2 proposition (bp+1,mp)
											else	if List.mem couleur1 proposition then  
														filtrer q1 q2 proposition (bp,mp+1)
													else filtrer q1 q2 proposition (bp,mp)								
	|_->false;;
#trace filtrer;;

let incr x = x:= !x + 1;;
let bp = 0 ;;
let mp = 0 ;;*)

let rec verifcombinaison list1 bla listreponse bp mp =
match (list1,bla,bp,mp) with
(*parcours simultanée des 2 liste
tete1 et queue1 pour la liste1 et tete2 et queue2 pour la listereponse*)
	| (list1,listreponse,5,5) -> true
	(*| list1,[] ->[]
	| [],listreponse -> []*)
	| (tete1::queue1,tete2::queue2,bp,mp) -> if tete1=tete2 then
												verifcombinaison queue1 queue2 listreponse (bp+1) mp
											else if List.mem tete1 listreponse then
													verifcombinaison queue1 queue2 listreponse bp (mp+1)
												else verifcombinaison queue1 queue2 listreponse bp mp
	|_->false;;
(*list2=[NULL];*)

(*let rec suppcombinaison list1 prop bp mp = 
	match list1 with
	| [] -> []
	| tete::queue-> if (verifcombinaison tete prop prop bp mp = 
					true)then 
					tete::(suppcombinaison prop queue bp mp) 
					else(suppcombinaison prop queue bp mp);;
*)
let rec suppcombinaison2 prop liste bp mp =
	match liste with
	| [] -> []
	| tete::queue -> if (verifcombinaison tete prop prop bp mp =
							true )then 
							tete::(suppcombinaison2 prop queue bp mp)
						else(suppcombinaison2 prop queue bp mp);;