(* Type checker statico *)
type 't env = (string * 't) list;;
let rec lookup x y = match x with
    | (i1,e1) :: x1 -> if y = i1 then e1 else lookup x1 y
    | [] -> failwith("wrong env")
;;

type ide = string;;
type exp =
      Eint of int
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

(* Valori esprimibili *)
type evT =
      Int
    | Bool
    | String 
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
    | Den s -> String
    | Dict l ->
        (match l with
            | [] -> DictValue
            | _ -> 
                let rec explore (list) =
                    (match list with
                        [] -> DictValue
                        | (k,v)::tail -> explore tail
                        | _ -> failwith("<list> non è una lista di coppie"))
                in explore l)
    | Insert (k,v,d) -> 
        (match (k,v,d) with
            | (ide, exp, dict) -> DictValue
            | (_, _, _)   -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | Delete (k,d) ->
        (match (k,d) with
            | (ide, dict) -> DictValue
            | (_, _) -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | HasKey (k,d) ->
        (match (k,d) with
            | (ide, dict) -> Bool
            | (_, _) -> failwith("<key> non è una stringa/<dict> non è un dizionario"))
    | Iterate (f,d) ->
        (match (f,d) with
            | (evFun, dict) -> DictValue
            | (_, _) -> failwith("<dict> non è un dizionario"))
    | Fold (f,d) ->
        (match (f,d) with
            | (evFun, dict) -> Int
            | (_, _) -> failwith("<funct> incompatibile/<dict> non è un dizionario"))
    | Filter (l,d) ->
        (match (l,d) with
            | (list, dict) -> DictValue
            | (_, dict) -> failwith("<list> non è una lista")
            | (_, _) -> failwith("<dict> non è un dizionario"))
    | _ -> failwith ("Non supportato")
;;

let typeCheck e = checker e [];;

typeCheck (Dict([]));;
typeCheck (Dict([("key",Eint(1))]));;
typeCheck (Dict([("key",Eint(1));("key2",Eint(2))]));;
typeCheck (Insert("key",Eint(5),Dict([])));;
typeCheck (Delete("key",Dict([])));;
typeCheck (HasKey("key",Dict([])));;
typeCheck (Iterate(Fun("y",Sum(Den "y",Eint(1))),Dict([])));;
typeCheck (Fold(Fun("y",Sum(Den "y",Eint(0))),Dict([])));;
typeCheck (Filter(["key"],Dict([("key",Eint(1));("key2",Eint(2))])));;