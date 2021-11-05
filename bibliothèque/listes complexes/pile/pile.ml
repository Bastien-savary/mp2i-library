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

(*definition d'un type comportant les fonctions précedentes pour simplifier les opérations avec les listes et tableaux*)
type 'a stack_imperative = {
    empty : unit -> bool;
    push : 'a -> unit;
    pop : unit -> 'a
};;

(* [stack_of_array t] retrourne une pile faite d'un tableau t *)
let stack_of_array t =
  let n = ref 0 in (* number of elemnts in the stack *)
  {
    empty = (fun () -> !n = 0);
    push = (fun e -> if !n >= Array.length t 
            then failwith "Full stack"
            else (t.(!n) <- e; incr n));
    pop = (fun () -> if !n = 0
          then failwith "Empty stack"
          else (decr n; t.(!n)))
  };;

(* [stack_to_list s] transforme une pile en liste *)
let stack_to_list s =
  let rec make_list () =
    if s.empty () then []
    else let top = s.pop () in
      top::make_list () in
  let l = List.rev (make_list ()) in
  List.iter s.push l;
  l;;
