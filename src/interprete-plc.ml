(* 
    User defined functions
*)
type funName = Fid of string;; (* identificativo della funzione *)
type funArg = Ide of string;; (* argomento della funzione *)
type funBody = Body of exp;; (* corpo (codice) della funzione *)
type funDef = Fun of funName * funArg * funBody;; (* definizione completa di una funzione *)
type funDecl = List of funDef;; (* lista delle definizioni di funzione *)

let rec getFunDef(n, l) = match (n, l) with
    | (Fid(s1), Fun(Fid(s2), Ide(af), Body(b))::ls) ->
        if s1 = s2 then
            Fun(Fid(s2), Ide(af), Body(b))
        else
            getFunDef(n, ls)
    | (Fid _, []) -> failwith("run-time error")
;; (* funName * funDecl -> funDef *)

let getFunArg f = match f with
    | Fun(Fid(nf), Ide(na), Body(b)) -> Ide(na)
;; (* funDef -> funArg *)

let getFunBody f = match f with
    | Fun(Fid(nf), Ide(na), Body(b)) -> Body(b)
;; (* funDef -> funBody *)

let getFunBodyExp f = match f with
    | Fun(Fid(nf), Ide(na), Body(b)) -> b
;;

(*
    Substitution: funzione che si occupa di sostituire le
    occorrenze di un identificatore nelle funzioni con il 
    suo valore effettivo. L'interprete non dovrà mai vedere
    un identificatore mentre valuta un'espressione.
*)

let rec subst(e1, arg, e2) = match e2 with (* funName, funArg, funBody *)
    | CstInt(n) -> CstInt(n)
    | CstTrue -> CstTrue
    | CstFalse -> CstFalse
    | Iszero(ea) -> Iszero(subst(e1, arg, ea))
    | Eq(ea, eb) -> Eq(subst(e1, arg, ea), subst(e1, arg, eb))
    | Times(ea, eb) -> Times(subst(e1, arg, ea), subst(e1, arg, eb))
    | Sum(ea, eb) -> Sum(subst(e1, arg, ea), subst(e1, arg, eb))
    | Sub(ea, eb) -> Sub(subst(e1, arg, ea), subst(e1, arg, eb))
    | And(ea, eb) -> And(subst(e1, arg, ea), subst(e1, arg, eb))
    | Or(ea, eb) -> Or(subst(e1, arg, ea), subst(e1, arg, eb))
    | Not(ea) -> Not(subst(e1, arg, ea))
    | Ifthenelse(ea,eb,ec) -> 
        Ifthenelse(
            subst(e1,arg,ea),
            subst(e1,arg,eb),
            subst(e1,arg,ec)
        )
    | Val(s) -> (match arg with | Ide(s) -> e1 | _ -> Val(s))
    | Apply(f, e) -> Apply(f, subst(e1, arg, e))
    | _ -> failwith("run-time error")
;;

(* 
    Sintassi del linguaggio: definisce le espressioni
    legali che possono essere utilizzate ed elaborate
    dall'interprete.
*)
type exp = (* grammatica del linguaggio *)
      CstInt of int         (* costante intera *)
    | CstTrue               (* costante booleana True *)
    | CstFalse              (* costante booleana False *)
    | Sum of exp * exp      (* somma di due espressioni *)
    | Sub of exp * exp      (* sottrazione di due espressioni *)
    | Times of exp * exp    (* prodotto di due espressioni *)
    | Iszero of exp         (* operatore di confronto con zero *)
    | Eq of exp * exp       (* operatore di uguaglianza tra espressioni *)
    | Or of exp * exp       (* operatore logico or tra espressioni *)
    | And of exp * exp      (* operatore logico and tra espressioni *)
    | Not of exp            (* operatore logico not tra espressioni *)
    | Ifthenelse of exp * exp * exp (* operatore condizionale if-then-else *)
    | Val of string         (* restituisce il valore associato ad un identificatore (?) *)
    | Apply of funName * exp (* chiamata di funzione *)
;;

(* 
    Expressible values: sono quei valori che possono essere
    restituiti come risultato di una valutazione su
    un'espressione (complessa).
*)
type evT =
      Int of int
    | Bool of bool
;;

(*
    Type checker del linguaggio: funzione ausiliaria dell'interprete
    che controlla il tipo dei valori restituiti dalla valutazione
    di espressioni. E' un typechecker dinamico.
*)
let typecheck (x, y) = (* input: nome del tipo, identificatore del tipo (?) *)
    match x with
        | "int" ->  (* se il nome del tipo è <int> *)
            (match y with
                | Int(u) -> true (* se il valore è effettivamente un int, ritorna true *)
                | _ -> false (* ritorna false altrimenti *)
            )
        | "bool" -> (* se il nome del tipo è <bool> *)
            (match y with
                | Bool(u) -> true (* se il valore è effettivamente un bool, ritorna true *)
                | _ -> false (* ritorna false altrimenti *)
            )
        | _ -> failwith ("not a valid type")
;;

(*
    Funzioni di valutazione ausiliare chiamate dall'interprete.
    Le operazioni di base utilizzano una regola di valutazione eager
    (tranne ifthenelse che usa una regola lazy (in base alla guardia)).
*)
let is_zero x = match (typecheck("int",x), x) with
    | (true, Int(y)) -> Bool(y=0)
    | (_, _) -> failwith("run-time error")
;;

let int_eq(x, y) = match (typecheck("int",x), typecheck("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Bool(v = w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let int_sum(x, y) = match (typecheck("int",x), typecheck("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v + w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let int_sub(x, y) = match (typecheck("int",x), typecheck("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v - w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let int_times(x, y) = match (typecheck("int",x), typecheck("int",y), x, y) with
    | (true, true, Int(v), Int(w)) -> Int(v * w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let bool_and(x, y) = match (typecheck("bool",x), typecheck("bool",y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v && w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let bool_or(x, y) = match (typecheck("bool",x), typecheck("bool",y), x, y) with
    | (true, true, Bool(v), Bool(w)) -> Bool(v || w)
    | (_, _, _, _) -> failwith("run-time error")
;;

let bool_not(x) = match (typecheck("bool",x), x) with
    | (true, Bool(v)) -> Bool(not(v))
    | (_, _) -> failwith("run-time error")
;;

let if_then_else(x, y, z) = match (typecheck("bool", x), x) with (* x = test-exp *)
    | (true, Bool(true)) -> y (* y = ramo then *)
    | (true, Bool(false)) -> z (* z = ramo else *)
    | (_, _) -> failwith("nonboolean guard")
    | _ -> failwith("run-time error")
;;

(*
    Interprete: si occupa di costruire l'albero di sinstassi astratta
    e di elaborare il programma scritto usando le regole di sintassi.
    Si tratta sostanzialmente di una funzione ricorsiva di valutazione
    che, appunto, valuta tutte un'espressione riducendo tutte le 
    sotto-espressioni in essa contenute fino a tirar fuori un valore 
    (che non è più un'espressione in quanto non deve più essere valutato).
*)
let rec eval (e : exp) : evT =
    match e with (* usa il pattern matching per valutare l'espressione *)
        | CstInt(n) -> Int(n) (* e = CstInt? Ritorna il suo valore *)
        | CstTrue -> Bool(true)
        | CstFalse -> Bool(false)
        | Iszero(e1) -> is_zero(eval(e1))
        | Eq(e1, e2) -> int_eq(eval(e1), eval(e2))
        | Sum(e1, e2) -> int_sum(eval(e1), eval(e2))
        | Sub(e1, e2) -> int_sub(eval(e1), eval(e2))
        | Times(e1, e2) -> int_times(eval(e1), eval(e2))
        | And(e1, e2) -> bool_and(eval(e1), eval(e2))
        | Or(e1, e2) -> bool_or(eval(e1), eval(e2))
        | Not(e1) -> bool_not(eval(e1))
        | Ifthenelse(e1, e2, e3) -> if_then_else(eval(e1), eval(e2), eval(e3))
;;

(* Esempio di espressioni possibili con exp *)
let e1 = Sum(CstInt(23), Times(CstInt(5), CstInt(6)));; (* 25 + 5 * 6 *)
let e2 = Eq(CstInt(5), CstInt(5));;
let e3 = Sub(CstInt(50), Sum(CstInt(7), CstInt(3)));;
let e4 = Ifthenelse(e2,e1,e3);;
let e5 = Or(CstInt(4), CstInt(5));;
let e6 = Not(CstTrue);;
let e7 = Or(Eq(CstInt(3), CstInt(6)), e2);;

(* Esempi di valutazioni di espressioni *)
eval e1;; (* 53 *)
eval e2;; (* true *)
eval e3;; (* 40 *)
eval e4;; (* 53 *)
eval e5;; (* run-time error *)
eval e6;; (* false *)
eval e7;; (* true *)

(*
    Interprete in grado di lavorare con le funzioni.
*)

let rec adv_eval (e, dcl) = match e with
    | CstInt(n) -> Int(n)
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Iszero(e1) -> is_zero(adv_eval(e1, dcl))
    | Eq(e1, e2) -> int_eq(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | Sum(e1, e2) -> int_sum(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | Sub(e1, e2) -> int_sub(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | Times(e1, e2) -> int_times(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | And(e1, e2) -> bool_and(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | Or(e1, e2) -> bool_or(adv_eval(e1, dcl), adv_eval(e2, dcl))
    | Not(e1) -> bool_not(adv_eval(e1, dcl))
    | Ifthenelse(e1, e2, e3) -> 
        if_then_else(
            adv_eval(e1, dcl), 
            adv_eval(e2, dcl), 
            adv_eval(e3, dcl)
        )
    | Val(s) -> failwith("run-time error: unbound name")
    | Apply(f, a) -> 
        let t = getFunDef(f, dcl) in 
            let es = subst(a, getFunArg(t), getFunBodyExp(t)) in
                eval(es, dcl)
    | _ -> failwith("run-time error")
;;
