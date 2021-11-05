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

(* [element t k] ] retrourne l'élément associé à la clé k dans la table de hachage*)
let element t k =
    let rec aux = function
    | [] -> failwith "not in the table"
    |(k',e)::q -> if k=k' then e
                else aux q in
aux t.donnees.(t.hache k);;

(* [ajout t k e] ajoute l'élément e dans la table associé à la clé k *)
let ajout t k e =
    if recherche t k == true then failwith "key already here"
    else t.donnees.(t.hache k) <- (k,e)::t.donnees.(t.hache k);;

(* [suppr t k] supprime la clé k de la table de hachage*)
let suppr t k =
    t.donnees.(t.hache k) <- List.filter (fun c -> fst c <> k) t.donnees.(t.hache k)
