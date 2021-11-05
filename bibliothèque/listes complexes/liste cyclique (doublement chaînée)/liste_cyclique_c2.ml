(*type de la liste cyclique*)
type 'a l2c = {elem : 'a; mutable prev : 'a l2c; mutable next : 'a l2c};;

(*pour créer une liste cyclique à 1 élément*)
let create e = 
      let rec l = {elem = e; prev = l; next = l} in l;;
      
(*[length l] renvoie la longueur de la liste l2c*)
let length l =
let rec aux l1 =
  if l1 == l then 1 else 1 + aux l1.next in 
  aux l.next;;

(* [print_l2c] renvoie les éléments de la liste l2c *)
let print_l2c l =
    let l_cur = ref l.next in 
    while !l_cur != l do 
        print_int !l_cur.elem; print_newline ();
        l_cur := !l_cur.next
        done;
        print_int !l_cur.elem;
        print_newline ();;
  
(*[appartient e l] indique si l'élément e appartien à l*)
let appartient e l =
  let cur = ref l.next in 
  while !cur.elem <> e && !cur != l do 
    cur := !cur.next
  done;
  !cur.elem = e;;

(*[add l e] pour ajouter ou [del l e] supprimer un élément d'une l2c en compléxité O(1)*)
let add l e = 
    let l_new = {elem = e; prev = l; next = l.next} in 
      l.next.prev <- l_new ; 
      l.next <- l_new;
      
let del l = 
    l.prev.next <- l.next;
    l.next.prev <- l.prev
    
(*[fusion l1 l2]  réalise la fusion des deux listes en une*)
let fusion l1 l2 = 
  (l1.next).prev <- l2;
  (l2.next).prev <- l1;
  let l1n = l1.next in 
  l1.next <- l2.next;
  l2.next <- l1n;;
  
  
