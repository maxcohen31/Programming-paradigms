open Base
open Stdio

(* 
  Return split_on_min : int list -> int list * int * int list che, 
  data una lista di interi lis non vuota, restituisce una tripla (lis1,m,lis2) in cui: 
    – m `e l’elemento minimo in lis,
    – mentre lis1 e lis2 sono liste che contengono gli elementi che precedono e seguono, rispettivamente,
  la prima occorrenza di m in lis, mantenendo l’ordine degli elementi.

  split_on_min [4;3;5;2;4;7;2;3] -> ([4;3;5] ,2 ,[4;7;2;3])

*)

(* find the minimum in a list *)
let rec find_min = function
  | [] -> failwith "Empty list"
  | [x] -> x
  | x :: tl -> let m = find_min tl in if m < x then m else x ;;

(* find the index of the given element in the list *)
let find_idx n lst =
  let rec aux l idx = 
    match l with
    | [] -> idx
    | x :: tl -> if x = n then idx else aux tl (idx + 1)
  in aux lst 0 ;;

(* Return a list with all the values between x and y *)
let select x y lst = 
  let rec aux l acc = 
    match l with 
    | [] -> failwith "Empty List"
    | hd :: tl -> if hd >= x && hd <= y then aux tl (x::acc) else aux tl acc
  in aux lst [] ;;

(* print a list *)
let print_int_list lst =
  printf "[";
  List.iter lst ~f:(fun x -> printf "%d " x);
  printf "]" ;;

(* 
   Returns a triple built as follow: 
     ([elements preceding minimum], minimum, [elements following minimum])
*)
let split_on_min lst = 
  let min = find_min lst in
    let min_idx = find_idx min lst in 
      let rec aux l acc1 acc2 idx = 
      match l with
      | [] -> failwith "Error: List is empty"
      | x :: tl -> if idx = min_idx then
                      (acc1, x, acc2 @ tl)
                  else if idx < min_idx then 
                      aux tl (x::acc1) acc2 (idx + 1)
                  else 
                      aux tl acc1 (x::acc2) (idx + 1)
  in 
  aux lst [] [] 0 ;;
  
                



let () =
  let (left, min_val, right) = split_on_min [4;3;5;2;4;7;2;3] in

  printf "Left: ";
  print_int_list left;
  printf "\n";

  printf "Min: %d\n" min_val;

  printf "Right: ";
  print_int_list right;
  printf "\n"



