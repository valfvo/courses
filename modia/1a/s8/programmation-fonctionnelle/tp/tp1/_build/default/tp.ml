open Graphics
open Affichage

(* Exercice 2 *)
(*  
   coeff_directeur : float*float -> float*float -> float
   calcule le coefficient directeur de la droite représentée par deux points
   Parametre (x1, y1) : float*float, le premier point
   Parametre (x2, y2) : float*float, le second point
   Resultat : float, le coefficient directeur de la droite passant par
   (x1, y1) et (x2, y2)
*)

let coeff_directeur (x1,y1) (x2,y2) = (y2 -. y1) /. (x2 -. x1)

let%test _ = coeff_directeur (0., 0.) (1., 2.) = 2.
let%test _ = coeff_directeur (1., 2.) (0., 0.) = 2.
let%test _ = coeff_directeur (0., 0.) (2., 1.) = 0.5
let%test _ = coeff_directeur (0., 0.) (-2., 1.) = -0.5
let%test _ = coeff_directeur (1., 2.) (2., 1.) = -1.


(* Exercice 3 *)
(* Les contrats et tests des fonctions ne sont pas demandés *)
(* f1 : int * int -> bool *)
let f1 (a, b) = (a + 1) = b
(* f2 : int -> bool *)
let f2 x = x >= 0
(* f3 : 'a -> `a *)
let f3 x = x
(* f4 : `a * `a -> bool *)
let f4 (a, b) = a < b
(* f5 : `a*`b -> `a*)
let f5 (a, _) = a

(* Exercice 4 *)
(* ieme : 'a * 'a * 'a -> int -> 'a *)
(* renvoie le ième élément d'un triplet *)
(* t : le triplet *)
(* i : l'indice de l'élèment dans le triplet *)
(* renvoie le ième élément t *)
(* précondition : 1 =< i =< 3 *)

let ieme t i =
  let (a, b, c) = t
  in if i = 1 then a else if i = 2 then b else c

let%test _ = ieme (5,60,7) 1 = 5
let%test _ = ieme (5,60,17) 2 = 60
let%test _ = ieme (5,60,17) 3 = 17
let%test _ = ieme ('r','e','l') 1 = 'r'
let%test _ = ieme ('r','e','l') 2 = 'e'
let%test _ = ieme ('r','e','l') 3 = 'l'


(* Exercice 5 *)
(* PGCD -> pgcd.ml *)

(* Exercice 6 *)
(*  padovan : int -> int
Fonction qui calcule la nième valeur de la suite de Padovan : u(n+3) = u(n+1) + u(n); u(2)=1, u(1)=u(0)=0 
Paramètre n : un entier représentant la nième valeur à calculer
Précondition : n >=0
Résultat : un entier la nième valeur de la suite de Padovan 
*)

let rec padovan n =
  if n < 2 then 0
  else if n = 2 then 1
  else (padovan (n-2)) + ((padovan (n-3)))

let%test _ = padovan 0 = 0
let%test _ = padovan 1 = 0 
let%test _ = padovan 2 = 1
let%test _ = padovan 3 = 0
let%test _ = padovan 4 = 1
let%test _ = padovan 5 = 1
let%test _ = padovan 6 = 1
let%test _ = padovan 7 = 2
let%test _ = padovan 8 = 2
let%test _ = padovan 9 = 3
let%test _ = padovan 10 = 4

(*
u3 = 0
u2 = 0
u1 = 1

for i in range(3, n+1)
  u = u2 + u3
  u3 = u2
  u2 = u1
  u1 = u
*)


let padovan2 n =
  let rec padovan2_aux i u1 u2 u3 =
    if i > n then u1
    else (padovan2_aux (i + 1) (u2 + u3) u1 u2)
  in if n < 2 then 0 else if n = 2 then 1 else padovan2_aux 3 1 0 0

let%test _ = padovan2 0 = 0
let%test _ = padovan2 1 = 0 
let%test _ = padovan2 2 = 1
let%test _ = padovan2 3 = 0
let%test _ = padovan2 4 = 1
let%test _ = padovan2 5 = 1
let%test _ = padovan2 6 = 1
let%test _ = padovan2 7 = 2
let%test _ = padovan2 8 = 2
let%test _ = padovan2 9 = 3
let%test _ = padovan2 10 = 4

(* Exercice 7 *)
(* estPremier : int -> bool
fonction qui indique si un nombre est premier
Paramètre n : un entier naturel dont on doit dire s'il est premier ou pas
Précondition : n >= 0
Résultat : l'information de si n est premier ou pas
*)

let estPremier n =
  let rec est_premier_aux k l =
    if l = 1 then true
    else if k mod l = 0 then false
    else (est_premier_aux k (l - 1))
  in if n < 4 then n > 1 else est_premier_aux n (n - 1)

let%test _ = estPremier 2
let%test _ = estPremier 3 
let%test _ = not (estPremier 4)
let%test _ = estPremier 5
let%test _ = not (estPremier 6)
let%test _ = estPremier 7
let%test _ = not (estPremier 8)
let%test _ = not (estPremier 9)
let%test _ = not (estPremier 10)
let%test _ = not (estPremier 0)
let%test _ = not (estPremier 1)

(*****************************)
(****** Bonus "ludique" ******)
(*****************************)


(*  Création de l'écran d'affichage *)
let _ = open_graph " 800x600"

(* Exercice 8 *)
(*  
   dragon : (int*int) -> (int*int) -> int -> unit
   Dessine la courbe du dragon à partir de deux points et d'une précision.
   Parametre (xa,ya) : (int*int), coordonnées de la première extrémité du dragon
   Paramètre (xb,yb) : (int*int), coordonnées de la seconde extrémité du dragon
   Paramètre n : nombre d'itération (plus n est grand, plus la courbe aura de détails)
   Resultat : unit, affichage de la courbe du dragon sur l'écran
   Précondition : n positif ou nul
*)

let rec dragon (xa,ya) (xb,yb) n =
  if n = 0 then (dessine_segment (xa, ya) (xb,yb))
  else
    let (xc, yc) = ((xa+xb)/2 + (yb-ya)/2, (ya+yb)/2 + (xa-xb)/2)
    in
      dragon (xa, ya) (xc, yc) (n-1);
      dragon (xb, yb) (xc, yc) (n-1)

let%test_unit _ = dragon (200,350) (600,350) 20;

(*  Fermeture de l'écran d'affichage *)
close_graph()
