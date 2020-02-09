(* Grammatica del linguaggio *)
type ide = string;;
type exp =
    | Eint of int
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
    | Dict of (ide * exp) list
    | Insert of ide * exp * exp (* key, value, dict *)
    | Delete of ide * exp (* key, dict *)
    | HasKey of ide * exp (* key, dict *)
    | Iterate of exp * exp (* funct, dict *)
    | Fold of exp * exp (* funct, dict *)
    | Filter of ide list * exp (* keyList, dict *)
;;

(* Ambiente *)
type 't env = ide -> 't;;
let emptyenv (v: 't) = function x -> v;;
let applyenv (ambiente : 't env) (identificatore : ide) = ambiente identificatore;;
let bind (ambiente : 't env) (identificatore : ide) (valore : 't) =
    function x -> if x = identificatore then valore else applyenv ambiente x;;

(* Valori esprimibili *)
type evT =
    | Int of int
    | Bool of bool
    | String of string
    | FunVal of evFun
    | RecFunVal of ide * evFun
    | Unbound
    (* Estensione dizionari *)
    | DictValue of (ide * evT) list
and evFun = ide * exp * evT env;;

(* Type checker dinamico *)
let typecheck (tipo : string) (valore : evT) : bool = match tipo with
    | "int" -> (match valore with
        | Int(_) -> true
        | _ -> false)
    | "bool" -> (match valore with
        | Bool(_) -> true
        | _ -> false)
    | "string" -> (match valore with
        | String(_) -> true
        | _ -> false)
    | "dict" -> (match valore with
		| DictValue(_) -> true
		| _ -> false)
	| _ -> failwith("Non è un tipo valido")
;;

(* Funzioni primitive *)
let sum x y  = if (typecheck "int"  x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n + u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let diff x y = if (typecheck "int"  x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n - u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let prod x y = if (typecheck "int"  x) && (typecheck "int" y)
    then (match (x,y) with
          (Int(n), Int(u)) -> Int(n * u)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let eq x y   = if (typecheck "int"  x) && (typecheck "int" y)
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

let minus x  = if (typecheck "int"  x)
    then (match x with
          Int(n) -> Int(-n)
        | _ -> failwith("Errore durante l'applicazione della funzione"))
    else failwith("Errore di tipo");;

let iszero x = if (typecheck "int"  x)
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

(*
    Funzione ausiliaria per la funzione <validate>, controlla se una 
    specifica chiave è già presente nella lista di coppie che andranno 
    inserite nel dizionario, in modo da evitare chiavi duplicate.
    @params: 
        <key>  : chiave da cercare
        <list> : lista in cui cercare la chiave
   @return: true se la chiave viene trovata, false altrimenti 
*)
let rec find key list = match list with
    | [] -> false
    | (k, _)::tail -> k = key || find key tail
;;

(*
    Funzione che permette di validare la lista di inizializzazione
    di un dizionario, in modo da controllare eventuali coppie con la 
    stessa chiave.
    Si è scelto di filtrare la lista piuttosto che lanciare un errore
    nel caso un cui venga trovata una chiave duplicata, per permettere
    comunque la creazione del dizionario inserendo soltanto l'ultima occorrenza
    della coppia con quella specifica chiave; tuttavia, è necessaria
    la modifica di una sola riga di codice per cambiare questo comportamento.
    In particolare, quando viene trovata una chiave duplicata si può scegliere 
    di continuare la validazione della lista, senza aggiungere la coppia
    incriminita, oppure lanciare una failwith ed interrompere la creazione
    del dizionario.
    @params:
        <list> : lista da validare, corrisponde a <initList>
    @return: una nuova lista filtrata, senza le eventuali coppie
        con la stessa chiave, mantenendo solo l'ultima occorrenza.
*)
let rec validate list = match list with
    | [] -> []
    | (k, v)::tail -> match find k tail with
        | true  -> tail (* failwith("<key> duplicata nella lista di inizializzazione") *)
        | false -> (k,v)::(validate tail)
;;

(*
    Funzione di ricerca in una lista. Equivalente a List.mem.
    Si differenzia dalla funzione <find> in quanto opera su liste
    di stringhe piuttosto che su liste di coppie.
    @params: 
        <key>  : chiave da cercare
        <list> : lista in cui cercare la chiave
   @return: true se la chiave viene trovata, false altrimenti 
*)
let rec exists (key : ide) (l : ide list) : bool = match l with
    | [] -> false 
    | k::tail -> k = key || exists key tail
;;

(* Interprete del linguaggio *)
let rec eval (e : exp) (ambiente : evT env) : evT = match e with
    | Eint n -> Int n
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
        else failwith ("<guardia> non booleana")
    | Let(i, e1, e2) -> eval e2 (bind ambiente i (eval e1 ambiente))
    | Fun(i, a) -> FunVal(i, a, ambiente)
    | FunCall(f, eArg) ->
        let fClosure = (eval f ambiente) in
        (match fClosure with
            | FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg (eval eArg ambiente))
            (* Per ottenere un linguaggio che funzioni con scoping statico, 
                la funzione deve essere valutata nel suo ambiente di dichiarazione "fDecEnv".
                Altrimenti, se si volesse utilizzare uno scoping dinamico, bisognerebbe 
                valutare la funzione nell'ambiente corrente "ambiente". *)
            | RecFunVal(g, (arg, fBody, fDecEnv)) ->
                let aVal = (eval eArg ambiente) in
                let rEnv = (bind fDecEnv g fClosure) in
                    let aEnv = (bind rEnv arg aVal) in
                    eval fBody aEnv
            | _ -> failwith("<valore> non di funzione"))
    | Letrec(f, funDef, lBody) ->
        (match funDef with
        | Fun(i, fBody) -> let r1 = (bind ambiente f (RecFunVal(f, (i, fBody, ambiente)))) in eval lBody r1
        | _ -> failwith("<definizione> non di funzione"))
    
    (* Estensione dizionari *)

    (*
        Costruttore del tipo dizionario (Dict). 
        Accetta come parametro una lista (eventualmente vuota) di coppie
        con le quali inizializzare il dizionario, altrimenti ne crea uno vuoto.
        Le coppie saranno controllate per mantenere la proprietà di unicità delle 
        chiavi all'interno del dizionario. Il modo con cui le coppie verranno controllate
        può essere mutato agendo sulla funzione <validate>: è possibile modificare il suo 
        comportamento quando viene trovata una chiave duplicata da "salto la chiave, lasciando
        l'occorrenza già trovata nel dizionario" a "errore, lancio una failwith", bloccando
        l'esecuzione del programma (vedere la specifica della funzione per maggiori dettagli).
        @params:
            <initList> : lista di coppie (<key>, <value>) (può anche essere vuota)
        @fail: se <key> è già presente in una qualche coppia all'interno di <initList>
        @fail: se <dict> non è un dizionario DictValue
        @return: un dizionario di tipo DictValue, eventualmente inizializzato con le coppie contenute in <initList>
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
                | (key, value)::tail -> 
                    if key = "" then failwith("<key> è una stringa vuota")
					else (key, eval value ambiente)::(evaluateList tail ambiente)
        in DictValue(evaluateList (validate initList) ambiente)
    
	(*
        Inserisce nel dizionario una coppia (<key>, <value>), 
        se non è già presente un'altra coppia con la stessa chiave.
        @params:
            <key>   : chiave della coppia
            <value> : valore della coppia
            <dict>  : dizionario in cui inserire la coppia
        @fail: se <key> è già presente in una qualche coppia all'interno di <dict>
        @fail: se <dict> non è un dizionario di tipo DictValue
        @return: un nuovo dizionario di tipo DictValue contenente la nuova coppia, 
			se non ne era già presente una con la stessa <key> nel dizionario iniziale
    *)
    | Insert(key, value, dict) -> (match eval dict ambiente with
        DictValue evaluatedDict ->
            if key = "" then failwith("<key> è una stringa vuota")
            else
                let rec insert (key : ide) (value : evT) (dict : (ide * evT) list) : (ide * evT) list =
                    (match dict with
                        | [] -> (key, value)::[] (* dizionario vuoto/chiave non presente -> inserisco la coppia *)
                        | (k, v)::tail ->
                            (* se ho trovato una chiave uguale, non inserisco la coppia *)
                            if (key = k) then failwith("<key> duplicata, non posso inserire la coppia") (*(k, v)::tail*)
                            (* altrimenti itero sul dizionario per cercare un'eventuale chiave già esistente,
                                inserendo, nel caso, la nuova coppia in fondo *)
                            else (k, v)::(insert key value tail))
                in DictValue(insert key (eval value ambiente) evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))
    
    (*
        Elimina dal dizionario la coppia che ha come chiave <key>, se presente.
        @params:
            <key>  : chiave da cercare nel dizionario ed eventualmente eliminare
            <dict> : dizionario da cui eliminare la coppia con chiave <key>
        @fail: se <dict> non è un dizionario di tipo DictValue
        @return : un nuovo dizionario di tipo DictValue senza la coppia con chiave <key>, 
			se era presente nel dizionario iniziale
    *)
    | Delete(key, dict) -> (match eval dict ambiente with
        DictValue evaluatedDict ->
            if key = "" then failwith("<key> è una stringa vuota")
            else
                let rec delete (key : ide) (dict : (ide * evT) list) : (ide * evT) list =
                    match dict with
                        | [] -> [] (* dizionario vuoto/chiave non trovata *)
                        | (k, v)::tail -> 
                            if (key = k) then tail (* se ho trovato la chiave, rimuovo la coppia *)
                            else (k, v)::(delete key tail) (* altrimenti continuo ad iterare *)
                in DictValue(delete key evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))

    (* 
        Controlla la presenza di una chiave <key> in un dizionario <dict>.
		@params:
			<key>  : chiave da cercare
			<dict> : dizionario in cui cercare la chiave
        @fail: se <dict> non è un dizionario di tipo DictValue
		@return: true se la chiave esiste nel dizionario, false altrimenti
    *)
    | HasKey(key, dict) -> (match eval dict ambiente with
        DictValue evaluatedDict ->
            if key = "" then failwith("<key> è una stringa vuota")
            else
                let rec contains (key : ide) (dict : (ide * evT) list) : bool =
                    match dict with
                        | [] -> false (* dizionario vuoto/chiave non presente, ritorno false *)
                        | (k, _)::tail -> 
                            if (key = k) then true (* chiave trovata, ritorno true *)
                            else contains key tail (* continuo a cercare *)
                in Bool(contains key evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))
	
	(*
		Applica la funzione <funct> ad ogni coppia presente nel dizionario <dict>.
		@params:
			<funct> : funzione da applicare ad ogni coppia del dizionario
			<dict>  : dizionare su cui applicare la funzione
        @fail: se <dict> non è un dizionario di tipo DictValue
		@return: un nuovo dizionario di tipo DictValue con i valori ottenuti come risultato della funzione
	*)
	| Iterate(funct, dict) -> (match eval funct ambiente, dict with
		FunVal(_, _, _), Dict evaluatedDict ->
			let rec apply (f : exp) (dict : (ide * exp) list) (ambiente : evT env) : (ide * evT) list =
				match dict with
					| [] -> []
					| (k, v)::tail -> (k, eval (FunCall(f, v)) ambiente)::(apply f tail ambiente)
			in DictValue(apply funct evaluatedDict ambiente)
        | _ -> failwith("<dict> non è un dizionario"))
	
	(*
		Applica la funzione <funct> sequenzialmente a tutti gli elementi del dizionario,
		calcolando un unico risultato.
		@params:
			<funct> : funzione da applicare sequenzialmente ad ogni coppia del dizionario
			<dict>  : dizionare su cui applicare la funzione
		@fail: se <funct> non è compatibile con il tipo di almeno un elemento
        @fail: se <dict> non è un dizionario di tipo DictValue
		@return: un unico valore, risultato della funzione <funct>
	*)
	| Fold(funct, dict) -> (match eval funct ambiente, dict with
		FunVal(_, _, _), Dict evaluatedDict ->
			let rec fold (f : exp) (dict : (ide * exp) list) (acc : evT) (ambiente : evT env) : evT =
				match dict with
					| [] -> acc
					| (_, v1)::tail -> 
                        match acc, (eval (FunCall(f, v1)) ambiente) with
                            | (Int(u), Int(v)) -> fold f tail (Int(u+v)) ambiente
                            | _ -> failwith("Errore durante l'applicazione della funzione <funct>, tipo incompatibile?")
				in fold funct evaluatedDict (Int(0)) ambiente
        | _ -> failwith("<dict> non è un dizionario"))
	
	(*
		Filtra il dizionario, restituendo un nuovo dizionario ottenuto eliminando dall'originale
		tutte le coppie la cui chiave non è presente in keyList.
		@params:
			<ketList> : lista di chiavi da mantenere nel nuovo dizionario
			<dict>    : dizionario da filtrare
        @fail: se <dict> non è un dizionario di tipo DictValue
		@return: un nuovo dizionario di tipo DictValue, 
			contenente solo le coppie la cui chiave appartiene a <keyList>
	*)
    | Filter(keyList, dict) -> (match eval dict ambiente with
      	DictValue evaluatedDict ->
            let rec filter (l : ide list) (dict : (ide * evT) list) : (ide * evT) list =
                    match dict with
                        | [] -> []
                        | (k, v)::tail -> 
                            if (exists k l) then (k, v)::(filter l tail)
                            else filter l tail
                in DictValue(filter keyList evaluatedDict)
        | _ -> failwith("<dict> non è un dizionario"))

;;

(* == TESTS == *)

(* Creazione ambiente, inizialmente vuoto *)
let myEnv = emptyenv Unbound;; 
(* val env0 : '_weakX -> evT = <fun> *)

(* Costruttore, con lista di inizializzazione valida *)
let myDict = Dict([
    ("mele",   Eint(430));
    ("banane", Eint(312));
    ("arance", Eint(525));
    ("pere",   Eint(217))
]);;
eval myDict myEnv;;
(* DictValue[("mele", Int 430); ... ;("pere", Int 217)] *)

(* Costruttore, chiave non unica *)
eval (Dict([("banane",Eint(20));("mele",Eint(30));("mele",Eint(40));("arance",Eint(50))])) myEnv;;
(* DictValue [("banane", Int 20); ("mele", Int 40); ("arance", Int 50)] *)

(* Costrutture, chiave vuota *)
eval (Dict([("",Eint(30));("mele",Eint(40))])) myEnv;;
(* Exception: Failure "<key> è una stringa vuota" *)

(* Insert, chiave non esistente *)
eval (Insert("kiwi", Eint(300), myDict)) myEnv;; 
(* [("mele", Int 430); ... ;("kiwi", Int 300)] *)

(* Insert, chiave esistente *)
eval (Insert("mele", Eint(550), myDict)) myEnv;; 
(* Exception: Failure "<key> duplicata, non posso inserire la coppia" *)

(* Insert, chiave vuota *)
eval (Insert("", Eint(720), myDict)) myEnv;;
(* Exception: Failure "<key> è una stringa vuota" *)

(* Delete, chiave esistente *)
eval (Delete("banane", myDict)) myEnv;;
(* DictValue [("mele", Int 430); ("arance", Int 525); ("pere", Int 217)] *)

(* Delete, chiave non esistente *)
eval (Delete("pesche", myDict)) myEnv;;
(* DictValue [("mele", Int 430); ("banane", Int 312); ("arance", Int 525); ("pere", Int 217)]) *)

(* Delete, chiave vuota *)
eval (Delete("", myDict)) myEnv;;
(* Exception: Failure "<key> è una stringa vuota" *)

(* HasKey, chiave esistente *)
eval (HasKey("arance", myDict)) myEnv;;
(* Bool true *)

(* HasKey, chiave non esistente *)
eval (HasKey("kiwi", myDict)) myEnv;;
(* Bool false *)

(* HasKey, chiave vuota *)
eval (HasKey("", myDict)) myEnv;;
(* Exception: Failure "<key> è una stringa vuota" *)

(* Iterate, funzione incremento (+1) *)
eval (Iterate(Fun("y", Sum(Den "y", Eint 1)), myDict)) myEnv;;
(* DictValue [("mele", Int 431); ("banane", Int 313); ("arance", Int 526); ("pere", Int 218)]) *)

(* Fold, funzione somma *)
eval (Fold(Fun("y", Sum(Den "y", Eint 0)), myDict)) myEnv;;
(* Int 1484 *)

(* Fold, funzione differenza *)
eval (Fold(Fun("y", Diff(Den "y", Eint 5)), myDict)) myEnv;;
(* Int 1464 *)

(* Fold, funzione prodotto *)
eval (Fold(Fun("y", Prod(Den "y", Eint 2)), myDict)) myEnv;;
(* Int 2968 *)

(* Fold, incompatibile *)
eval (Fold(Fun("y", Or(Den "y", Ebool false)), myDict)) myEnv;;
(* Exception: Failure "Errore di tipo" *)

(* Filter *)
eval (Filter(["mele"; "pere"], myDict)) myEnv;;
(* DictValue [("mele", Int 430); ("pere", Int 217)] *)

(* Filter, con una chiave inesistente *)
eval (Filter(["mele";"pesche"], myDict)) myEnv;;
(* DictValue [("mele", Int 430)] *)

(* Filter, con tutte le chiavi inesistenti *)
eval (Filter(["kiwi";"pesche"], myDict)) myEnv;;
(* DictValue [] *)

(* Filter, con una lista vuota *)
eval (Filter([], myDict)) myEnv;;
(* DictValue [] *)
