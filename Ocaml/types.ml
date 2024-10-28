type data = int*int*int ;;

(* Record *)
type 2d_point = {x:float, y:float} ;;

let p = { x = 3.; y = -4. } ;;

let quadrante {x = x_pos; y = y_pos } =
    match x_pos>=0.,y_pos>=0. with
    | true,true -> 1
    | false,true -> 2
    | false,false -> 3
    | true,false -> 4 ;;

quadrante {x=(-3.); y=2.} ;;


type persona = { nome: string; cognome: string; età: int; indirizzo: string; citta: string } ;;

let mario = { nome="Mario"; cognome="Rossi"; età=33; indirizzo="Dé"; città="Pisa" } ;;

let bianca = { mario with nome = "Bianca"; età = 19} ;; (* ATTENZIONE: crea un altro record dato che mario è immutabile *)

type numero_testo = 
    | Txt of string 
    | Num of int ;; 

let x = Txt "34" ;;
let y = Num 26 ;;


let converter x = 
    match x with
    | Txt x -> int_of_string x
    | Num x -> string_of_int x ;;


let int_ext = 
    | Num of int
    | Nan
    | Plus_inf
    | Min_inf


let sum x y =
    match x,y with
    | NaN,_ | _,NaN -> NaN
    | Plus_inf,Minus_inf | Minus_inf,Plus_inf -> NaN
    | Plus_inf,_ | _,Plus_inf -> Plus_inf
    | Minus_inf,_ | _,Minus_inf -> Minus_inf
    | Num n1,Num n2 -> Num (n1+n2) ;;

(* La funzione massimo è polimorfa, in quanto l'unica operazione che viene eseguita sugli elementi 
   della lista che prende come parametro è l'operazione di confronto > 
   (che si applica a valori di qualunque tipo) *)
let rec massimo lis =
    match lis with
    | [] -> None   
    | x::lis' ->  match massimo lis' with
                    | None -> Some x 
                    | Some max -> if x>max then Some x
                                    else Some max ;;


(* tipi ricorsivi *)
type lista_di_int =
    | Nil
    | Elem of int * lista_di_int ;;
    
let lst = Elem (3 ,Elem (4, Elem (6,Nil))) ;;


(* alberi *)
type albero_bin = 
  | Nodo of int*albero_bin*albero_bin 
  | Foglia of int ;;

let rec previsit t = 
    match t with
    | Foglia v -> v 
    | Nodo(v,sx,dx) -> v::((previsit sx)@(previsit dx)) ;; 



(* abstract syntax tree *)
type op = Add | Sub | Mul | Div | Mod ;;

type exp =
    | Val of int
    | Op of op*exp*exp
    | UMin of exp ;;

let exp1 = Op (Sub, (Op (Mul, Val 3, Val 7)), Val 5) ;; (* 3 * 7 - 5*)
let exp2 = Op (Mul, UMin (Val 3), (Op (Sub, Val 7, Val 5))) ;; (* -3 * 7 - 5 *)

