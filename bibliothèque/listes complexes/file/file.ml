(*type d'une file*)
type 'a file = {tab: 'a array; mutable deb : int; mutable fin: int; mutable vide : bool}

(*[ajoute f e] ajoute l'élément e à la fin de la file d'attente f*)
let ajoute f e = 
  if f.deb = f.fin && not f.vide then failwith "File pleine"
  else (f.tab.(f.fin) <- e;
        f.fin <- (f.fin + 1) mod Array.length f.tab;
        f.vide <- false);;
        
(*[retire f] retire l'élément en tête de la file d'attente f et le renvoie*)
let retire f  = 
  if f.vide then failwith "File vide"
  else let res = f.tab.(f.deb) in
          (f.deb <- (f.deb + 1) mod Array.length f.tab;
          f.vide <- f.deb = f.fin;
          res);;
 
