(* 
   IMPLEMENTAZIONE DI UN AMBIENTE IN OCAML 
*)


let emptyenv = [] ;;

(* aggoirnamento ambiente s con associazione (x, y) *)
let bind s x v = (x, v)::s ;; 

(* operazione di referencing *)
let rec lookup s x = 
    match x with
    | [] -> failwith "not found"
    | (y, v)::r -> if (x = y) then v else lookup r x ;;


(* 
   IMPLEMENTAZIONE ALTERNATIVA - AMBIENTE COME FUNZIONE POLIMORFA 
*)

(* ambiente polimorfo *)
type 't env = ide -> 't ;; (* 't sarà il tipo dei valori esprimibili *)

(*ambiente vuoto *)
let empyenv = fun x -> Unbound (* valore speciale *)

s x ;;

(* aggiornamento ambiente s con associazione (x, v) *)
let bind s x v = fun i -> if (i = x ) then v else (s i) ;;
