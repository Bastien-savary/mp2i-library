(* [somme_t t1] renvoie la somme des elements de t1 *)
let somme_t t1 = 
    let result = ref 0 in
    for i=0 to ((Array.length t1)-1) do
        result := t1.(i) + !result
    done;
    !result;;

(* [appartient_t e t1] renvoie si e appartient a t1*)
let appartient_t e t1 = 
    let result = ref false in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i) = e then result := true
    done;
    !result;;

(* [mini_t t1] renvoie le minimum de t1*)
let mini_t t1 = 
    let result = ref max_int in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i) < !result then result := t1.(i)
    done;
    !result;;

(* [maxi_t t1] renvoie le maximum de t1*)
let maxi_t t1 = 
    let result = ref min_int in
    for i = 0 to ((Array.length t1)-1) do
        if t1.(i)> !result then result := t1.(i)
    done;
    !result;;

(* [croissant_t t1] renvoie si t1 est triée dans l'ordre croissant *)
let croissant_t t1 =
    let result = ref true in
    for i = 0 to ((Array.length t1)-2) do
        if t1.(i) > t1.(i+1) then result := false
    done;
    !result;;

(* [decroissant_t t1] renvoie si t1 est triée dans l'ordre decroissant  *)
let decroissant_t t1 = 
    let result = ref true in
    for i = 0 to ((Array.length t1)-2) do
        if t1.(i) < t1.(i+1) then result := false
    done;
    !result;;
    
(* [doublon_t t1] indique si il existe un doublon dans t1 *)
let doublon_t t1 =
    let result = ref false in
    for i = 0 to ((Array.length t1)-2) do
        for j = i + 1 to ((Array.length t1)-1) do
        if t1.(i)=t1.(j) then result := true
        done;
    done;
    !result

(* [inverser_l] renvoie l'inversion du tableaux t1*)
let reverse_t t1 =
    let n = ((Array.length t1)-1) in 
    for i = 0 to n/2 do
        let c = ref t1.(i) in 
        let d = ref t1.(n-i)in
        t1.(i) <- !d;
        t1.(n-i) <- !c;
    done;
    t1;;

(* [egal_t t1 t2] indique si les deux tableaux sont identiques*)
let equal_t t1 t2 = 
    let result = ref true in
    let m1 = Array.length t1 in
    let m2 = Array.length t2 in
        if m1 <> m2 then result := false
        else for i = 0 to m1-1 do
            if t1.(i) <> t2.(i) then result := false
            done;
    !result;;

(* [consecutive_max_t t1] retourne la plus importante somme d'éléments du tableau t1 *)
let consecutive_max_t t1 =
    let m = ref t1.(0) in
    let m_cur = ref t1.(0) in
    for i = 1 to Array.length t1 - 1 do
        m_cur := max (!m_cur + t1.(i)) t1.(i);
        m := max !m !m_cur
    done;
    !m;;

(* [image_t f t1] renvoie le tableau contenant les images de chaque element de t1 par f *)           
let image_t f t1 =
    for i = 0 to ((Array.length t1)-1) do
        t1.(i) <- f(t1.(i))
    done;
    t1;;

  
(* [dichotomie_t e t1] indique si e appartient à t1 en compléxité O(nlog(n))*)
let dichotomie_t e t1 = 
    let rec aux i j =
        if i > j then false
        else let m = (i+j)/2 in
            if t1.(m) = e then true
            else if t1.(m) > e then aux i (m-1)
            else aux (m+1) j in
aux 0 ((Array.length t1)-1);;

(* [trichotomie_t e t1] indique si e appartient à t1 en compléxité O(nlog(n))*)
let trichotomie_t e t1 =
    let rec aux i j =
    if i > j then false
    else let m1 = (2*i + j + 1)/3 in
        let m2 =(i + 2*j +2)/3 in
        if t1.(m1) = e || t1.(m2) = e then true
        else if e< t1.(m1) then aux i (m1-1)
        else if e< t1.(m1) then aux (m1+1) (m2-1)
        else aux (m2+1) j in
aux 0 ((Array.length t1)-1);;

(* [tri_comptage_t  t1] trie le tableau t1*)
let tri_comptage_t t1 =
    let m = maxi_t t1 in
    let compte = Array.make (m+1) 0 in
    let n = Array.length t1 in
    for i=0 to n - 1 do
        compte.(t1.(i)) <- compte.(t1.(i)) + 1
    done;
    let k = ref 0 in 
    for i=0 to m do
        for j=1 to compte.(i) do
            t1.(!k) <- i;
            incr k
        done;
    done;
    t1;;
    
(* [swap_t t i j] echange t.(i) et t.(j) *)    
let swap_t t1 i j =
  let tmp = t1.(i) in
  t1.(i) <- t1.(j);
  t1.(j) <- tmp;;
  
(* [tri_t t] tri le tableau t1 en compléxité (O(n**2)) *)
let tri_t t1 = 
  for _ = 0 to Array.length t1 - 1 do
    for j = 0 to Array.length t1 - 2 do
      if t1.(j) > t1.(j + 1) then swap_t t1 j (j + 1)
      done
  done;;

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
