(* [somme_t t1] renvoie la somme des éléments de t1 *)
let somme_t t1 = 
    let result = ref 0 in
    for i=0 to ((Array.length t1)-1) do
        result := t1.(i) + !result
    done;
    !result;;

(* [appartient_t e t1] renvoie si e appartient à t1*)
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

(* [croissant_t t1] renvoie si t1 est trié dans l'ordre croissant *)
let croissant_t t1 =
    let result = ref true in
    for i = 0 to ((Array.length t1)-2) do
        if t1.(i) > t1.(i+1) then result := false
    done;
    !result;;

(* [decroissant_t t1] renvoie si t1 est trié dans l'ordre décroissant  *)
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

(* [dichotomie_t e t1] indique si e appartient à t1 en compléxité O(nlog(n))*)
let dichotomie e t1 = 
    let rec aux i j =
        if i > j then false
        else let m = (i+j)/2 in
            if t1.(m) = e then true
            else if t1.(m) > e then aux i (m-1)
            else aux (m+1) j in
aux 0 ((Array.length t1)-1);;

(* [trichotomie_t e t1] indique si e appartient à t1 en compléxité O(nlog(n))*)
let trichotomie e t1 =
    let rec aux i j =
    if i > j then false
    else let m1 = (2*i + j + 1)/3 in
        let m2 =(i + 2*j +2)/3 in
        if t1.(m1) = e || t1.(m2) = e then true
        else if e< t1.(m1) then aux i (m1-1)
        else if e< t1.(m1) then aux (m1+1) (m2-1)
        else aux (m2+1) j in
aux 0 ((Array.length t1)-1);;
