let list = ["rouge";"vert";"bleu";"jaune";"blanc";"orange";"noir";"violet"];;

let rec construire taille = 
 let rec construirepossi list = match list with
 | [] -> []
 | t::q -> (("rouge"::t)::(("vert"::t)::(("bleu"::t)::(("jaune"::t)::(("blanc"::t)::(("orange"::t)::(("noir"::t)::(("violet"::t)::(construirepossi q)))))))))

in match taille with
1 -> [["rouge"];["vert"];["jaune"];["bleu"];["violet"];["blanc"];["noir"];["orange"]]
|_-> construirepossi (construire  (taille-1));;
 
 #trace construire;;
 construire 5;;