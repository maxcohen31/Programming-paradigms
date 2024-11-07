let rec genera_lista n = 
    match n with
    | 0 -> [] 
    | x -> if (x < 0) then [] else genera_lista(x-1) @ [x] ;;


et rec media lis = 
    match lis with
    | [] -> 0.0
    | _ ->  let somma = List.fold_left (+) 0 lis
            in float_of_int somma /. float_of_int (List.length lis);;


let rec take lis n =
    match (lis, n) with
    | ([], _) -> []
    | (_, 0) -> []
    | (x::lis', y) -> if (y < 0) then [] else x :: take lis' (y-1);;


let rec drop lis n =
    match (lis, n) with 
    | ([], _) -> []
    | (_, 0) -> lis
    | (_::lis', y) -> if (y < 0) then lis else drop lis' (y-1) ;;



let somma_costante lis =
    match lis with
    | [] -> true
    | (a, b)::lis' -> let prima_somma = a + b in (* prendo la somma della prima coppia *)
        let rec verifica lis'' = 
            match lis'' with
            | [] -> true (* controlla tutte le coppie *)
            | (x, y)::lis''' -> if ((x+y) = prima_somma) then verifica lis''' else false in verifica lis' ;;



let ord lista = 
    match lista with
    | [] -> true 
    | x::lista' -> let primo_elemento = x in (* prendo il primo elemento *)
         let rec verifica lista'' = 
         match lista'' with 
         | [] -> true
            (* verifico che il resto della lista sia ordinato *)
         | y::lista''' -> if (y > primo_elemento) then verifica lista''' else false in verifica lista' ;;



let rec min x lista = 
    match lista with
    | [] -> x
    | first::lista' -> let minimum = min first lista' in if (x < minimum) then x else minimum ;;


let rec remove x lis = 
    match lis with
    | [] -> []
    | first::lis' -> if first = x then lis' (*s e x è il promi elemento ritorna la lista *)
    else first :: remove x lis' ;; (* altrimenti chiama remove sul resto della lista *)


let rec sort lis = 
    match lis with 
    | [] -> []
    | x::lis' -> let min_val = min x lis in min_val :: sort (remove min_val lis) ;; 


let rec set lis = 
    match lis with
    | [] -> true
    | x::lis' -> if (List.mem x lis') then false else set lis' ;;
