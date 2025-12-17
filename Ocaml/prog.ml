open Base
open Stdio

(* 
    Definire una funzione intersezione che riceve due liste di interi come argomenti e
    restituisce una lista che contiene gli elementi presenti in entrambe le liste ricevute.
*)

let intersezione lis1 lis2 = List.filter ~f:(fun x -> List.mem lis2 x ~equal:Int.equal) lis1 ;;

let intersezione2 lis1 lis2 = 
    let rec aux lst1 lst2 acc = 
        match lst1 with
        | [] -> lst2 @ acc
        | x :: tl1 -> if List.mem lis2 x ~equal:Int.equal then x :: acc else aux tl1 lst2 acc 
    in aux lis1 lis2 [] ;;

(* 
    Definire una funzione doppio che riceve una lista come argomento e restituisce true se
    ogni elemento della lista (eccetto il primo) è pari al doppio dell’elemento che lo precede. Restituisce false altrimenti.
*)
let rec doppio lis1 = 
    match lis1 with
    | [] -> false
    | x :: [] -> if x % 2 = 0 then true else false 
    | x :: (y :: _ as tl) -> if (y = 2*x) then doppio tl else false ;;

(*
    Definire una funzione sonodispari che riceve una lista come argomento e restituisce
    true se la lista contiene un numero dispari di elementi. Restituisce false altrimenti. La funzione
    non deve contare il numero degli elementi (ossia non deve calcolare la lunghezza della lista, ne
    usare una funzione ausiliaria che faccia questo conto).
*)

(* returns the index of the last element - Forbidden!*)
(* let last lis = 
    let rec aux l idx = 
        match l with
        | [] -> idx
        | rest -> aux rest (idx + 1) 
    in aux lis 0 ;;

let sonodispari lis = let idx = last lis in idx % 2 = 1 ;; 
*)

(* correct version *)
let rec sonodispari2 lis = 
    match lis with
    | [] -> false
    | [_] -> true
    | _ :: _ :: tl -> sonodispari2 tl ;;

let rec alternati lst = 
    match lst with
    | [] -> true
    | _ :: [] -> true
    | _ :: _ :: [] -> true
    | x :: _ :: (z :: _ as tl) -> if x = z then alternati tl else false ;;


(*
    Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
    controllasegni : int list -> bool
    che, data una lista lis di interi, verifica che ogni coppia di elementi consecutivi diversi da zero
    contenga valori con lo stesso segno.
*)

(* STDLIB: | x::tl -> let (b, p) = List.fold_left f (true, x) tl in b *)
let controllasegni lis = 
    (* f sarà usata nella fold_left *)
    let f (bool, prev) current = 
        if ((bool && prev < 0 && current < 0) || (bool && prev > 0 && current > 0)) then (true, prev)
        else (false, current)
    in match lis with
    | [] -> true
    | _ :: [] -> true
    | x :: tl -> let (b, _) = List.fold tl ~init:(true,x) ~f:f  in b ;; 


(*
    Definire una funzione ricorsiva CAML
    canc: ‘a list -> ‘a -> ‘a list
    Tale che (canc lis n) cancella da lis l’ultima occorrenza del valore n. 
    Se n non compare la lista viene restituita immutata  
*)

let canc lis n = 
    let rec aux l idx =
        match l with
        | [] -> l
        | x :: [] -> if x = n then [] else [x]
        | x :: tl -> let rev_l = List.rev tl in 
                     if x = n then List.rev rev_l else List.rev (x :: aux rev_l (idx + 1))
    in List.rev (aux lis 0) ;;

(* Stesso funzione ma senza ricorsione esplicita - O(n^2)*)
let canc lis n = 
    (* f sarà la funzione utilizzata nella fold *)
    let f (acc, found) current =
        (* prima occorrenza di n. La segno e vado avanti *)
        if current = n && not found then (acc, true) 
        (* ritrovo n: aggiungo la prima occorrenza *)
        else if current = n && found then (acc @ [n], true) 
        (* elemento diverso da n e n: aggiungo current al risultato *)
        else if current <> n && found then (acc @ [current], false)
        (* salto l'n-esima occorrenza di n e aggiungo solo l'elemento corrente*)
        else (acc @ [current], false)
    in
    match lis with
    | [] -> lis
    | tl -> let (res, _) = List.fold tl ~init:([], false) ~f:f  in res ;; 


        
        
let l1 = [3; 5; 4; 8] ;;
let l2 = [1; 3; 6; 3; 0; 7] ;;
let res = intersezione l1 l2 ;;
let res2 = intersezione2 l1 l2 ;;
let res_doppio = doppio [2; 4; 8; 16; 32] ;;
let res_doppio2 = doppio [1; 2; 3] ;;
let dispari = sonodispari2 [1; 2; 3] ;;
let alt = alternati [3; 5; 3; 5; 3; 5] ;;
let alt2 = alternati [1; 2; 1; 2; 5] ;;
let alt3 = alternati [4; 8; 4] ;;

let () =
    List.iter res ~f:(fun x -> printf "res: %d \n"  x);;
    List.iter res2 ~f:(fun x -> printf "res2: %d \n" x) ;;
    printf "%b\n" res_doppio ;;
    printf "%b\n" res_doppio2 ;;
    printf "%b\n" dispari ;;
    printf "%b\n" alt ;;
    printf "%b\n" alt2 ;;
    printf "%b\n" alt3 ;;
