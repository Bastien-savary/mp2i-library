(* [element n l1] renvoie le n-ieme element de l1  *)
let rec element n l1 = match l1 with
    |[] -> failwith "impossible"
    |e1::q -> if n = 0 then e1
                else element (n-1) q;;

(* [length_l l1] renvoie la longueur de l1*)
let rec length_l l1 = match l1 with
    |[] -> 0
    |e::q -> 1 + length_l q;;

(* [somme_l l1] renvoie la somme des elements de l1 *)
let rec somme_l l1 = match l1 with
    | [] -> 0
    | e::q -> e + somme_l q;;

(* [appartient_l e l1] rindique si e appartient a l1*)     
let rec appartient_l e l1 = match l1 with
    |[] -> false
    |e1::q -> e1 = e || appartient_l e q;;

(* [mini_l l1] renvoie le minimum de l1*)
let rec mini_l l1 = match l1 with
    |[] -> max_int
    |e::q -> let m = mini_l q in
            if e < m then e
            else m;;

(* [maxi_l l1] renvoie le maximum de l1*)            
let rec maxi_l l1 = match l1 with
    |[] -> min_int
    |e::q -> let m = maxi_l q in
            if e > m then e
            else m;;

(* [croissant_l l1] indique si l1 est triée dans l'ordre croissant *)
let rec croissant_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e>e1 || croissant_l (e1::q);;

(* [decroissant_l l1] indique si l1 est triée dans l'ordre decroissant  *)
let rec decroissant_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e<e1 || decroissant_l (e1::q);;

(* [doublon_l l1] indique si il existe un doublon dans l1 *)
let rec doublon_l l1 = match l1 with
    |[] -> false
    |e::q -> if appartient_l e q then true
                else doublon_l q;;

(* [egal_l l1 l2] indique si les deux listes sont identiques*)
let rec egal_l l1 l2 = match l1,l2 with
    |[],[] -> true
    |[],_ | _,[]-> false
    |e::q, e1::q1 -> if e <> e1 then false 
                    else egal_l q q1;;
