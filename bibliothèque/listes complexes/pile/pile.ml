(*type d'une pile (stack)*)
type 'a stack = {t : 'a array; mutable n : int}

(*[stack_push p e] permet d'ajouter un élément e au dessus de la pile *)
let stack_push p e = 
    if p.n >= Array.length p.t
    then failwith "Pile pleine"
    else (p.t.(p.n) <- e; p.n <- p.n + 1);;

(*[stack_pop p e] permet d'enlever l'élément e au dessus de la pile *)
let stack_pop p = 
    if p.n = 0 then failwith "Pile vide"
    else (p.n <- p.n - 1; p.t.(p.n));;
