(* 
    Realizzazione di un interprete in Ocaml
*)


(* Sintassi operazioni algebriche *)
let op = Add | Sub | Mul | Div ;;
type exp = 
    | Val of int
    | Op of op*exp*exp ;;


let expression1 = Op (Sub (Op (Mul , Val 3, Val 7)), Val 5) ;; (* (3*7)-5 *)
let expression2 = Op (Add (Op (Div, Val 6, Val 3)), Val 10) ;; (* 10 + (6 / 3)*)

(* Funzione che trasforma l'albero di sintassi astratta in forma testuale *)
let rec to_string exp = 
    let symbol o =
        match o with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
    in match e with
        | Val n -> string_of_int n
        | Op (o, e1, e2) -> "(" ^ (to_string e1) ^ (symbol o) ^ (to_string e2) ^ ")" ;;

(* SCANNER - tokenizzatore *)
(* rappresentiamo i token *)
type token = 
    | Tkn_NUM of int
    | Tkn_OP of string
    | Tkn_LPAR (* simbolo ( *)
    | Tkn_RPAR (* simbolo ) *)
    | Tkn_END  (* fine espressione *)

(* ancora non ci sono controlli grammaticali: )(3++(88 è corretta per lo scanner *)

exception ParseError of string*string ;;

let tokenize s = 
    (* 
       funzione che scandisce ricorsivamente s,
       dove pos è la posizione del carattere corrente
     *)
    let rec tokenize_rec s pos = 
        if pos=String.length s then [Tkn_END] (* caso base: fine lista *)
        else 
            let c = String.sub s pos 1 (* estrare primo carattere *)
            in 
            (* si richiama ricorsivamente prima di gestire il carattere corrente *)
            match c with
            | " " -> tokens
            | "(" -> Tkn_LPAR::tokens
            | ")" -> Tkn_RPAR::tokens
            | "+" | "-" | "*" | "/" -> (Tkn_OP)::tokens
            | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                (* accorpa cifre consecutive *)
                (match tokens with
                | Tkn_NUM n::tokens' -> Tkn_NUM (int_of_string (c^(string_of_int n)))::tokens'
                | _ -> Tkn_NUM (int_of_string c)::tokens)
                | _ -> raise (ParseError ("Tokenizer","unknown symbol: "^c))
            in
            tokenize_rec s 0 ;;

(* esempio d'uso dello scanner *)
let t1 = tokenize "(10 + 16) - (31 * 4)" ;;


(* PARSER *)
(*
    - controlla che l'espressione sia sintatticamente corretta
    - genera AST

    Come si realizza un parser:
        - la grammatica deve essere resa non ambigua
        - se il linguaggio è semplice, si implementa un parser a discesa ricorsiva
        - se il linguaggio non è molto semplice, si usa un parser generator
*)
let parse s =
    ,→
    (* usiamo un riferimento per scandire la lista dei token ( ottenuta da tokenize ) *)
    let tokens = ref (tokenize s) in
    (* restituisce il primo token senza rimuoverlo *)
    let lookahead () = 
        match !tokens with
        | [] -> raise (ParseError ("Parser","lookahead error"))
        | t::_ -> t
    in

    (* elimina il primo token *)
    let consume () = match !tokens with
        | [] -> raise (ParseError ("Parser","consume error"))
        | t::tkns -> tokens := tkns
    in

    (* funzioni mutuamente ricorsive che seguono dalla grammatica *)
    (* Exp ::= Term [ + Exp | - Exp ] *)
    let rec exp () =
        let t1 = term() in
        match lookahead () with
        | Tkn_OP "+" -> consume(); Op (Add,t1,exp())
        | Tkn_OP "-" -> consume(); Op (Sub,t1,exp())
        | _ -> t1
    (* Term ::= Factor [ + Term | - Term ] *)
    and term () =
        let f1 = factor() in
            match lookahead() with
            | Tkn_OP "*" -> consume(); Op (Mul,f1,term())
            | Tkn_OP "/" -> consume(); Op (Div,f1,term())
            | _ -> f1
    (* Factor ::= n | ( Exp ) *)
    and factor () =
        match lookahead() with
        | Tkn_NUM n -> consume(); Val 
        | Tkn_LPAR -> consume(); let e = exp() in
        (match lookahead() with
        | Tkn_RPAR -> consume(); e
        | _ -> raise (ParseError ("Parser","RPAR error"))
        )
        | _ -> raise (ParseError ("Parser","NUM/LPAR error")) ;;

(* utilizzo *)
(*let ast = parse "32 + 24 * 12 * (3-1) +2" ;;*)

(* interprete con semantica big step *)
let rec eval e = 
    match e with
    | Val n -> Val N
    | (op, e1, e2) -> 
            match (eval e1, eval e2) with
            | (Val n1, Val n2) -> (match op with 
                                   | Add -> Val (n1+n2) 
                                   | Sub -> Val (n1-n2)
                                   | Mul -> Val (n1*n2)
                                   | Div -> Val (n1/n2)
                                    )
            _ -> failwith "Errore: impossibile che si verifichi" ;;





