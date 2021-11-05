(*type d'une liste simplement chainée*)
type 'a case = {elem : 'a; mutable next : 'a liste1 } 
      and 'a liste1 = Vide | C of 'a case

(*[to_list l] convertit la liste simplement chaînée en liste classique*)
let rec to_list l = match l with 
    |Vide -> []
    |C(c1) -> c1.elem :: (to_list c1.next)

(*[] indique si la liste possède un cycle*)
let has_circle l = 
        let vu = [] in 
        let rec aux l2 vu = match l2 with 
            |Vide -> false
            |C (case) -> if List.mem case vu then true else aux case.next (case::vu) in aux l vu;
            
 
