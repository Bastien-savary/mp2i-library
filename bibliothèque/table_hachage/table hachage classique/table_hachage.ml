(*type de la table de hachage*)
type ('a, 'b) table_hachage = {hache : 'a -> int; donnees : ('a*'b) list array; largeur:int};;

(*[create h w] permet de creer une table de hachage*)
let create h w = {hache = h; donnees = Array.make w []; largeur = w};;

(*[appartient t k] indique si la clé k appartient à la table de hachage*)
let appartient t k =
    let rec aux = function
    | [] -> false
    |(k',_)::q -> k=k' || aux q in 
    aux t.donnees.(t.hache k);;

let appartient_2 t k =
    t.donnees.(t.hache k)
    |> List.map fst 
    |> List.mem k;;

(* [element t k] ] retrourne l'élément associé à la clé k dans la table de hachage*)
let element t k =
    let rec aux = function
    | [] -> failwith "la clé k n'existe pas"
    |(k',e)::q -> if k=k' then e
                else aux q in
aux t.donnees.(t.hache k);;

(* [ajout t k e] ajoute l'élément e dans la table associé à la clé k *)
let ajout t k e =
    if not (recherche t k) then 
        t.donnees.(t.hache k) <- (k, e)::t.donnees.(t.hache k)
        else failwith "la clé est déja présente"

(* [suppr t k] supprime la clé k de la table de hachage*)
let suppr t k =
    t.donnees.(t.hache k) <- List.filter (fun c -> fst c <> k) t.donnees.(t.hache k)
    
let suppr_2 t k =
    if not (appartient t k) then failwith "pas de changement"
    else let rec enlever = function 
        | [] -> []
        | (k', _):: q when k = k'  -> enlever q 
        |e::q -> e::enlever q in 
        t.donnees.(t.hache k) <- enlever (t.donnees.(t.hache k))    
 
