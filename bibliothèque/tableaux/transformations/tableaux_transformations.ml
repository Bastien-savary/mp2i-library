(* [swap_t t i j] échange t.(i) et t.(j) *)    
let swap_t t1 i j =
  let tmp = t1.(i) in
  t1.(i) <- t1.(j);
  t1.(j) <- tmp;;

(* [inverser_t t1] renvoie l'inversion du tableaux t1*)
let reverse_t t1 =
    let n = ((Array.length t1)-1) in 
    for i = 0 to n/2 do
        let c = ref t1.(i) in 
        let d = ref t1.(n-i)in
        t1.(i) <- !d;
        t1.(n-i) <- !c;
    done;
    t1;;

(* [Image_t f t1] renvoie le tableau contenant les images de chaque élément de t1 par f *)           
let map_t f t1 =
    for i = 0 to ((Array.length t1)-1) do
        t1.(i) <- f(t1.(i))
    done;
    t1;;

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
