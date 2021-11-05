(* [concat_l l1 l2] renvoie la concatenation de l1 et l2 *)
let rec concat_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> e::concat_l q l2;;

(* [tri_doublon_l l1] renvoie l1 sans les doublons *)
let rec tri_doublon_l l1 = match l1 with
    |[] -> []
    |e::q -> if appartient_l e q then tri_doublon_l q
            else e::tri_doublon_l q;;

(* [Image_l f l1] renvoie la liste contenant les images de chaque element de l1 par f *)           
let rec image_l f l1 = match l1 with
    | [] -> []
    |e::q -> f(e)::image_l f q;;

(* [split_l l1] partage la liste l1 en deux listes *)
let rec split_l l1 = match l1 with
    |[] -> ([],[])
    |[e] -> ([e],[])
    |e1::e2::q -> let q1, q2 = split_l q in
                    e1::q1, e2::q2;;

(* [fusion_l l1 l2] fusionne deux listes pour en former une unique triée *)
let rec fusion_l l1 l2 = match l1, l2 with
    |[],_ -> l2
    |_,[] -> l1
    |e1::q1, e2::q2 -> if e1< e2 then e1::fusion_l q1 l2
                        else e2::fusion_l l1 q2;;

(* [tri_l l1] trie la liste l1, utilise split_l et fusion_l *)
let rec tri_l l1 = match l1 with
    |[] -> []
    |[e] -> [e]
    |_ -> let l2, l3 = split_l l1 in
        fusion_l (tri_l l2) (tri_l l3);;

(*[partition l p] renvoie deux listes, l2 contient les elements inférieurs à p et l3 les éléments supérieurs ou égaux à p*)
let rec partition_l l1 pivot = match l1 with
    |[] -> [],[]
    |e::q -> let l2,l3 = partition_l q pivot in
                if e< pivot then e::l2, l3
                else l2, e::l3

(* [quicksort_l l1] trie la liste l1 mais en compléxité moindre *)
let rec quicksort_l l1 = match l1 with
    |[]->[]
    |p::q -> let l2, l3 = partition_l q p in
         append_l (quicksort_l l2) (p::quicksort_l l3)

(* [ajoute_l e ll] insère l'élément e dans chaque liste contenue dans ll *)
let rec ajoute_l e ll = match ll with
    | [] -> []
    | x::s -> (e::x)::ajoute_l e s;;

(* [array_of_list l] transforme une liste en tableau *)
let array_of_list l = 
    let m = length_l l in
    let t = Array.make m 0 in
    let rec aux m1 l1 = match l1 with
        |[]->[||]
        |e::q -> t.(m-m1) <- e;
                if m1 = 1 then t
                else aux (m1 - 1) q in
aux m l;;

(* [array_to_list t] transforme un tableau en liste *)
let array_to_list t = 
    let m = Array.length t in
    let rec aux i = 
        if i = 0 then []
        else t.(m-i)::aux (i-1) in
aux m;;
