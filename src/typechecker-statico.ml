(* Type checker statico *)

type ide = string;;
type exp =
    | Eint of int
    | Ebool of bool
    | Den of ide
    | Ide of ide
    | Estring of string
    | Prod of exp * exp
    | Sum of exp * exp
    | Diff of exp * exp
    | Eq of exp * exp
    | Minus of exp
    | IsZero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp
    | FunCall of exp * exp
    | Letrec of ide * exp * exp
    (* Estensione dizionari *)
    | Dict of (ide * exp) list
    | Insert of ide * exp * exp (* key, value, dict *)
    | Delete of ide * exp (* key, dict *)
    | HasKey of ide * exp (* key, dict *)
    | Iterate of exp * exp (* funct, dict *)
    | Fold of exp * exp (* funct, dict *)
    | Filter of ide list * exp (* keyList, dict *)
;;

(* Ambiente *)
type 't env = (string * 't) list;;

let rec lookup x y = match x with
    | (i1,e1) :: x1 -> if y = i1 then e1 else lookup x1 y
    | [] -> failwith("wrong env")
;;

let bind (env : 't env) (i : ide) (v : 't) = function 
    x -> if x = i  then v
         else lookup env x
;;

(* Valori esprimibili *)
type evT =
    | Int
    | Bool
    | Unbound
    | FunVal of evFun 
    | RecFunVal of ide * evFun
    (* Estensione dizionari *)
    | DictValue 
and evFun = ide * exp * evT env
;;

let rec checker (e : exp) (env : evT env) : evT = 
    match e with
    | Eint i -> Int
    | Ebool b -> Bool
    | Sum (e1, e2) -> 
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    (Int, Int) -> Int
                    | _ -> failwith("Errore di tipo"))
    | Diff(e1, e2) -> 
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    | (Int, Int) -> Int
                    | _ -> failwith("Errore di tipo"))
    | Prod(e1, e2) ->
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    | (Int, Int) -> Int
                    | _ -> failwith("Errore di tipo"))
    | Minus(e1) ->
        let t1 = checker e1 env in
            (match t1 with
                | Int -> Int
                | _ -> failwith("Errore di tipo"))
    | Eq(e1, e2) -> 
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    | (Int, Int) -> Bool
                    | _ -> failwith("Errore di tipo"))
    | IsZero(e1) ->
        let t1 = checker e1 env in
            (match t1 with
                | Int -> Bool
                | _ -> failwith("Errore di tipo"))
    | Or(e1, e2) -> 
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    (Bool, Bool) -> Bool
                    | _ -> failwith("Errore di tipo"))
    | And(e1, e2) ->
        let t1 = checker e1 env in
            let t2 = checker e2 env in
                (match t1, t2 with
                    (Bool, Bool) -> Bool
                    | _ -> failwith("Errore di tipo"))
    | Not(e1) -> 
        let t1 = checker e1 env in
            (match t1 with
                | Bool -> Bool
                | _ -> failwith("Errore di tipo"))
    | Ifthenelse(e1, e2, e3) -> (match checker e1 env with
        | Bool -> 
            let t2 = checker e2 env in
                let t3 = checker e3 env in
                    if t2 = t3 then t2
                    else failwith("branch <if> e branch <else> hanno tipi differenti")
        | _ -> failwith("<guardia> non booleana"))
    | Dict(l) ->
        (let rec explore (list) = (match list with
            | [] -> DictValue
            | (k,v)::tail -> explore tail)
        in explore l)
    | Insert (k,v,d) -> 
        (match (k,v,checker d env) with
            | (ide, exp, DictValue) -> DictValue
            | (_, _, _)   -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | Delete (k,d) ->
        (match (k,checker d env) with
            | (ide, DictValue) -> DictValue
            | (_, _)   -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | HasKey (k,d) ->
        (match (k,checker d env) with
            | (ide, DictValue) -> Bool
            | (_, _) -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | Iterate (f,d) ->
        (match (f,checker d env) with
            | (evFun, DictValue) -> DictValue
            | (_, _) -> failwith("<dict> non è un dizionario"))
    | Fold (f,d) ->
        (match (f,checker d env) with
            | (evFun, DictValue) -> Int
            | (_, _) -> failwith("<funct> incompatibile/<dict> non è un dizionario"))
    | Filter (l,d) ->
        (match (l,checker d env) with
            | (key::tail, DictValue) -> DictValue
            | (_, DictValue) -> failwith("<list> non è una lista")
            | (_, _) -> failwith("<dict> non è un dizionario"))
    | _ -> failwith ("Non supportato")
;;

let typeCheck e = checker e [];;

typeCheck (Dict([]));; 
(* DictValue *)

typeCheck (Dict([("key",Eint(1))]));;
(* DictValue *)

typeCheck (Dict([("key",Eint(1));("key2",Eint(2))]));;
(* DictValue *)

typeCheck (Insert("key",Eint(5),Dict([])));;
(* DictValue *)

typeCheck (Delete("key",Dict([])));;
(* DictValue *)

typeCheck (HasKey("key",Dict([])));;
(* Bool *)

typeCheck (Iterate(Fun("y",Sum(Den "y",Eint(1))),Dict([])));;
(* DictValue *)

typeCheck (Fold(Fun("y",Sum(Den "y",Eint(0))),Dict([])));;
(* Int *)

typeCheck (Filter(["key"],Dict([("key",Eint(1));("key2",Eint(2))])));;
(* DictValue *)
