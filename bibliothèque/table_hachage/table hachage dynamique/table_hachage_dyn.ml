(*type de la table hacahge dynamique*)
type ('a, 'b) table_dyn = {
                hache : int -> 'a -> int; 
                mutable taille : int;
                mutable donnees: ('a * 'b) list array;
                mutable largeur: int};;
                
(*[creer_dyn h] permet de creer une table de hachage dynamique vide et de largeur initiale 1*)
let creer_dyn h = {
                hache = h; 
                 taille= 0;
                donnees= [|[]|];
                largeur= 1
    }  

(*[rearrange t w2] réarrange la table de hachage sur la largeur w2*)
let rearrange t w2 =
    let d = Array.make w2 [] in 
    let rec ajout q = match q with 
        |[] -> ()
        |(a, b)::tl -> d.(t.hache w2 a) <- (a, b) :: d.(t.hache w2 a);
                                ajout tl in 
        for i = 0 to t.largeur - 1 do 
            ajout t.donnees.(i)
            done; 
        t.donnees <- d;
        t.largeur <- w2;;
