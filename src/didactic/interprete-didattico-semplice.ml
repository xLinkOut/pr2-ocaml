(* Interprete di un semplice linguaggio funzionale con ambiente *)

type variable = string ;;
type constant = int ;;
type operand = Plus | Minus | Times | Div ;;

(* Le espressioni *)
type exp = 
  | Cnst of constant
  | Var of variable
  | Op of exp * operand * exp
  | Let of variable * exp * exp
;;

(* Funzioni del run-time *)
(* Environment *)
let emptyenv = [] ;; (* the empty environment *)
let rec lookup env x =
    match env with 
    | []        -> failwith("Variable not found")
    | (y, v)::t -> if x = y then v else lookup t x 
;;

(* Decodifica delle operazioni di base *)
let eval_op (v1 : int) (op : operand) (v2 : int) : int =
  match v1, op, v2 with 
    | _, Plus, _ -> v1 + v2 
    | _, Minus, _ -> v1 - v2 
    | _, Times, _ -> v1 * v2 
    | _, Div, _ -> v1 / v2 
;;


(* Ciclo dell'interprete *)
let rec eval (e : exp) (env : (variable * int) list) : int =
  match e with
    | Cnst i -> i
    | Var x  -> lookup env x 
    | Op (e1, op, e2) -> 
        let v1 = eval e1 env in
          let v2 = eval e2 env in
            eval_op v1 op v2
    | Let (x, erhs, ebody) ->           
        let xval = eval erhs env in
              let env1 = (x, xval) :: env in
                eval ebody env1
;;

let run e = eval e [] ;;
