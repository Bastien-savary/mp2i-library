(*type d'un zipper*)
type 'a zipper = { left : 'a list; right : 'a list};;

(*déplacer le curseur d'un zipper vers la [move_right z] droite ou la [move_left z] gauche*)
let move_right z = match z.right with 
  |[] -> failwith "empty right"
  |e::q -> {left = e::z.left; right = q};;
  
 let move_left z = match z.left with 
  |[] -> failwith "empty left"
  |e::q -> {left = q; right = e::z.right};;
  
(*[move_r n z] déplace le curseur n fois vers la droite, ou vers la gauche avec [move_l n z]*)
let rec move_r n z = 
    if n = 0 then z 
    else move_r (n-1) (move_right z);;

let rec move_l n z =
    if n = 0 then z 
    else move_l (n-1) (move_left z);;
    
(* [add e z] ajoute un élément à droite du zipper et [del z] supprime l'élément à droite du zipper*)
let add e z = {left = z.left; right = e :: z.right};;  

let del z = match z.right with 
    |[]-> failwith "liste droite vide"
    |e::q -> {left = z.left; right = q};;
    
(*[zipper_to_list z] transforme le zipper z en liste*)
let zipper_to_list z = 
    List.rev z.left @ z.right 
