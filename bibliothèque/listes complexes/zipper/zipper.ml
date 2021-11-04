(*type d'un zipper*)
type 'a zipper = { left : 'a list; right : 'a list};;

(*déplacer le curseur d'un zipper vers la droite ou la gauche*)
let move_right z = match z.right with 
  |[] -> failwith "empty right"
  |e::q -> {left = e::z.left; right = q};;
  
 let move_left z = match z.left with 
  |[] -> failwith "empty left"
  |e::q -> {left = q; right = e::z.right};;
