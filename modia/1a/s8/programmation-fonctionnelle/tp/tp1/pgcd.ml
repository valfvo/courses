(*  Exercice à rendre **)

(* pgcd : int -> int -> int *)
(* calcule le pgcd de a et b*)
(* a : int, premier nombre *)
(* b : int, deuxieme nombre *)
(* resultat: int, le pgcd de a et b *)
(* precondition : a >= 0 et b >= 0 *)
(* postcondition : pgcd >= 0 *)
let rec pgcd a b =
  if a = b then a
  else if a > b then (pgcd (a - b) b)
  else (pgcd a (b - a))

let%test _ = pgcd 0 0 = 0
let%test _ = pgcd 11 17 = 1
let%test _ = pgcd 42 30 = 6
