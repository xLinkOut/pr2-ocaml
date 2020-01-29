(* Grammatica del linguaggio *)
type ide = string;;
type exp =
      Eint of int
    | Ebool of bool
    | Den of ide
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
    | Estring of string
    | Dict of (ide * exp) list
    | Insert of ide * exp * exp
    | Delete of exp * ide
    | HasKey of ide * exp
    | Iterate of exp * exp
    | Fold of exp * exp
    | Filter of ide list * exp;;


(* Ambiente *)
type 't env = ide -> 't;;
let emptyenv (v: 't) = function x -> v;;
let applyenv (ambiente : 't env) (identificatore : ide) = ambiente identificatore;;
let bind (ambiente : 't env) (identificatore : ide) (valore : 't) =
    function x -> if x = identificatore then valore else applyenv ambiente x;;

(* Valori esprimibili *)
type evT =
      Int of int
    | Bool of bool
    | String of string
    | Unbound
    | FunVal of evFun
    | RecFunVal of ide * evFun
    (* Estensione dizionari *)
    | DictValue of (ide * evT) list

and evFun = ide * exp * evT env;;

(* Type checker dinamico *)
let typecheck (tipo : string) (valore : evT) : bool = match tipo with
      "int" -> (match valore with
          Int(_) -> true
        | _      -> false)
    | "bool" -> (match valore with
          Bool(_) -> true
        | _       -> false)
    | _ -> failwith("Non e' un tipo valido");;


(* Funzioni primitive *)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n * u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let sum x y  = if (typecheck "int" x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n + u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n - u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let eq x y   = if (typecheck "int" x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Bool(n = u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;


let vel x y  = if (typecheck "bool" x) && (typecheck "bool" y)
    then (match (x,y) with
          (Bool(b), Bool(e)) -> Bool(b || e)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let et x y   = if (typecheck "bool" x) && (typecheck "bool" y)
    then (match (x,y) with
          (Bool(b), Bool(e)) -> Bool(b && e)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let minus x  = if (typecheck "int" x)
    then (match x with
          Int(n) -> Int(-n)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let iszero x = if (typecheck "int" x)
    then (match x with
          Int(n) -> Bool(n=0)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;
let non x    = if (typecheck "bool" x)
    then (match x with
          Bool(true)  -> Bool(false)
        | Bool(false) -> Bool(true)
        | _ -> failwith("Errore di tipo"))
    else failwith("Errore di tipo");;

(* Interprete del linguaggio *)
let rec eval (e : exp) (ambiente : evT env) : evT = match e with
      Eint n -> Int n
    | Ebool b -> Bool b
    | IsZero a -> iszero (eval a ambiente)
    | Den i -> applyenv ambiente i
    | Eq(a, b) -> eq (eval a ambiente) (eval b ambiente)
    | Prod(a, b) -> prod (eval a ambiente) (eval b ambiente)
    | Sum(a, b) -> sum (eval a ambiente) (eval b ambiente)
    | Diff(a, b) -> diff (eval a ambiente) (eval b ambiente)
    | Minus a -> minus (eval a ambiente)
    | And(a, b) -> et (eval a ambiente) (eval b ambiente)
    | Or(a, b) -> vel (eval a ambiente) (eval b ambiente)
    | Not a -> non (eval a ambiente)
    | Ifthenelse(a, b, c) ->
        let g = (eval a ambiente) in
        if (typecheck "bool" g) then 
            (if g = Bool(true) then (eval b ambiente) else (eval c ambiente))
        else failwith ("non boolean guard")
    | Let(i, e1, e2) -> eval e2 (bind ambiente i (eval e1 ambiente))
    | Fun(i, a) -> FunVal(i, a, ambiente)
    | FunCall(f, eArg) ->
        let fClosure = (eval f ambiente) in
        (match fClosure with
            | FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg (eval eArg ambiente))
            (* in order to obtain static scope the function has to be evaluated with its declaration environment 'fDecEnv';
            to obtain dynamic scope the function has to be evaluated with the current environment 'ambiente' *)
            | RecFunVal(g, (arg, fBody, fDecEnv)) ->
                let aVal = (eval eArg ambiente) in
                let rEnv = (bind fDecEnv g fClosure) in
                    let aEnv = (bind rEnv arg aVal) in
                    eval fBody aEnv
            | _ -> failwith("non functional value"))
    | Letrec(f, funDef, bodyOfLet) ->
        (match funDef with
        | Fun(i, fBody) -> let r1 = (bind ambiente f (RecFunVal(f, (i, fBody, ambiente)))) in eval bodyOfLet r1
        | _ -> failwith("non functional def"))
    | Estring s -> String s
    (* Estensione dizionari *)

    (*
        Costruttore del tipo dizionario (Dict). 
        Accetta come parametro una lista (eventualmente vuota) di coppie
        con le quali inizializzare il dizionario, altrimenti ne crea uno vuoto.
        Le coppie saranno controllate per mantenere la proprietà di unicità delle 
        chiavi all'interno del dizionario.
        @params:
            <initList> : lista di coppie (<key>, <value>) (può anche essere vuota)
        @fail: se <dict> non è un dizionario DictValue
        @fail: se <key> è già presente in una qualche coppia all'interno di <initList>
        @return: un dizionario (DictValue), eventualmente inizializzato con le coppie contenute in <initList>
    *)
    | Dict(initList) ->
        (*
            Funzione (ausiliaria) di valutazione della lista di inizializzazione per il dizionario.
            @params:
                <initList> : lista di coppie (<key>, <value>) (può anche essere vuota)
                <ambiente> : ambiente di valutazione
            @fail: se <key> è già presente in una qualche coppia all'interno di <initList>
            @return: una lista di coppie (<key>, <value>) valide
        *)
        let rec evaluateList (initList : (ide * exp) list) (ambiente : evT env) : (ide * evT) list =
            match initList with
        
                | [] -> [] (* se la lista è vuota, inizializzo un dizionario vuoto *)
                
                (* se la lista iniziale non è vuota, compongo il dizionario ricorsivamenete aggiungengo
                    le coppie con chiave <key> e come valore la valutazione di <value> nell'ambiente attuale, 
                    fino ad esaurire le coppie in initList *)
                | (key, value)::listaRimanente -> 
                    (key, eval value ambiente)::(evaluateList listaRimanente ambiente)
                    (* ^ qui va fatto il controllo *)
        
        (* ritorno un DictValue, che appartiene ai tipi esprimibili *)
        in DictValue(evaluateList initList ambiente)
    (*
        Inserisce nel dizionario una coppia (<key>, <value>), 
        se non è già presente un'altra coppia con la stessa chiave.
        @params:
            <key>   : chiave della coppia
            <value> : valore della coppia
            <dict>  : dizionario in cui inserire la coppia
        @fail: se <dict> non è un dizionario DictValue
        @fail: se <key> è già presente in una qualche coppia all'interno di <dict>
        @return: un nuovo dizionario contenente la nuova coppia, se non ne era già presente
            una con la stessa <key> nel dizionario iniziale
    *)
    | Insert(key, value, dict) -> (match eval dict ambiente with
        DictValue evaluatedDict ->
            let rec insert (key : ide) (value : evT) (dict : (ide * evT) list) : (ide * evT) list =
                (match dict with
                    (* se il dizionario è vuoto/non esiste una chiave con lo stesso nome, 
                        inserisco la coppia richiesta *)
                    | [] -> (key, value)::[]
                    | (k, v)::t ->
                        (* se ho trovato una chiave uguale, non inserisco la coppia *)
                        if (key = k) then failwith("<key> duplicata, non posso inserire la coppia") (*(k, v)::t*)
                        (* altrimenti itero sul dizionario per cercare un'eventuale chiave già esistente,
                            inserendo, nel caso, la nuova coppia in fondo al dizionario *)
                        else (k, v)::(insert key value t))
            in DictValue(insert key (eval value ambiente) evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))
    
    (*
        Elimina dal dizionario la coppia che ha come chiave <key>, se presente.
        @params:
            <key>  : chiave da cercare nel dizionare ed eventualmente eliminare
            <dict> : dizionario da cui eliminare la coppia con chiave <key>
        @fail: se <dict> non è un dizionario DictValue
        @return : un nuovo dizionario senza la coppia con chiave <key>, se era presente nel 
            dizionario iniziale
    *)

    | Delete(dict,key) -> (match eval dict ambiente with
        | DictValue evaluatedDict ->
            let rec delete (key : ide) (dict : (ide * evT) list) : (ide * evT) list =
                match dict with
                    | [] -> [] (* dizionario vuoto/chiave non trovata *)
                    | (k, v)::t -> 
                        if (key = k) then t (* se ho trovato la chiave, rimuovo la coppia *)
                        else (k, v)::(delete key t) (* altrimenti continuo ad iterare *)
            in DictValue(delete key evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))

    (* 
        Controlla se una chiave <key> esiste in un certo dizionario <dict>:
        ritorna True in caso affermativo, False altrimenti.
        <key>  : chiave da cercare
        <dict> : dizionario in cui cercare la chiave
    *)
    | HasKey(key,dict) -> match eval dict ambiente with
          DictValue v ->
            let rec contains (key : ide) (dict : (ide * evT) list) : bool =
                match dict with
                    | [] -> false (* dizionario vuoto/chiave non presente, ritorno false *)
                    | (k, _)::t -> 
                        if (key = k) then true (* chiave trovata, ritorno true *)
                        else contains key t (* continuo ad iterare *)
            in Bool(contains key v)
        | _ -> failwith("<dict> non è un dizionario")
;;

(* == DICT TESTS == *)

(* Creo un ambiente inizialmente vuoto *)
Printf.printf "Creo un ambiente inizialmente vuoto\n";;
let myEnv = emptyenv Unbound;; (* val env0 : '_weakX -> evT = <fun> *)

(* Creo un nuovo dizionario inizializzandolo con alcuni elementi *)
Printf.printf "Creo un nuovo dizionario inizializzandolo con alcuni elementi\n";;
let myDict = Dict([
    ("mele",   Eint(430));
    ("banane", Eint(312));
    ("arance", Eint(525));
    ("pere",   Eint(217))
]);;
eval myDict myEnv;;
(* - : evT = DictValue[("mele", Int 430); ... ;("pere", Int 217)] *)

let myDictWrong = Dict([
    ("mele", Eint(12));
    ("mele", Eint(13))
]);;
eval myDictWrong myEnv;;
(* duplicated key error *)

(* Insert *)
eval (Insert("kiwi", Eint(300), myDict)) myEnv;; 
(* [("mele", Int 430); ... ;("kiwi", Int 300)] *)

(* Duplicate insert *)
eval (Insert("mele", Eint(550), myDict)) myEnv;; 
(* Exception: Failure "<key> duplicata, non posso inserire la coppia". *)

(* Delete *)
(*eval (Delete("banane", myDict)) myEnv;;
(* Delete su una chiave non esistente *)
eval (Delete("pesche", myDict)) myEnv;;*)