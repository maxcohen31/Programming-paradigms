let rec genera_lista n = 
    match n with
    | 0 -> []
    | x -> genera_lista (x - 1) @ [x];;


let rec media lis = 
    match lis with
    | [] -> 0.0
    | _ -> let s = List.fold_left (+.) 0.0 lis in  (s /. (float_of_int (List.length lis)));;
    


let rec take lis n = 
    match n with
    | 0 -> []
    | x -> match lis with
                | [] -> failwith "Error: Empty list"
                | x::lis' -> [x] @ (take lis' (n-1));;
                

let rev lis =
    let rec rev_aux lis1 lis2 = 
        match lis1 with
        | [] -> lis2
        | x::lis1' -> rev_aux lis1' (x::lis2)
    in rev_aux lis [] ;;

let drop lis n =
    let rec aux_drop lis' acc =
        match lis' with
        | [] -> [] (* lista vuota *)
        | _ when acc >= n -> lis' (* una volta rimosso un elemento restituisce il risultato *) 
        | x::l -> aux_drop l (acc + 1) (* continua a rimuovere elementi *)
    in aux_drop lis 0 ;;

(* Più semplice xd *)
let rec drop2 lis n =
  match (lis, n) with
  | [], _ -> []
  | l, 0 -> l
  | _ :: tl, n -> drop tl (n - 1)
;;
            

let rec somma_costante l = 
    match l with
    | [] -> true
    | [(x, y)] -> true
    | (x,y)::(x1, y1)::l' -> let res1 = x + y in let res2 = x1 + y1 
        in if res1 <> res2 then false else somma_costante ((x1, y1)::l');;
    
let rec ord l = 
    match l with
    | [] -> true
    | [x] -> true
    | x::y::l' -> if x > y then false else ord (y::l') ;;

let rec min lis x = 
    match lis with
    | [] -> x
    | t::l' -> let res = if t < x then t else x in min l' res ;;



let rec remove lis x = 
    match lis with
    | [] -> [] 
    | n::l' -> if n = x then l' else n :: remove l' x;;

let rec sort lis = 
  match lis with
  | [] -> []
  | head :: tail ->
      let m = min tail head in
      m :: sort (remove (head::tail) m);; 
    
let rec setp lis =
    match lis with 
    | [] -> true
    | [x] -> true
    | x::y::l' -> if x <> y then setp l' else false ;;
    

let rec multisetp lis = 
    match lis with 
    | [] -> true
    | (x, n)::l' -> if (List.exists (fun (y, _) -> y = x) l') then false else multisetp l'  ;;
    


let rec crea_multiset lis = 
    match lis with
    | [] -> []
    | x::l' -> let c = List.length (List.filter (fun y -> y = x) l') in
               let r = List.filter (fun y -> x <> y) l' in
               let res = (x, c+1) :: crea_multiset r in
               if multisetp res then res else failwith "Error: duplicate element" ;;
    
crea_multiset [10; 6; 3; 6; 4; 5; 4; 5; 5] ;;


