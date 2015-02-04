(** ************************************************************************** **)
(**                                                                            **)
(**  	KINDSYSTEM (9): Offers functions to calculate the dual of a contract.    **)
(**									     																										   **)
(** ************************************************************************** **)

(*----------------------------------------------------
   OCAML TOPLEVEL IMPORTS (for Emacs only)

#use "errors.ml";;
#use "tipi.ml";;
#use "extTipi.ml";;
#load "str.cma";;
#load "dynlink.cma";;
#load "camlp4o.cma";;
#use "python.ml";;

----------------------------------------------------*)

(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *)
open Tipi;;
open ExtTipi;;
open Python;; 

type 'a partial = Some of 'a | None ;;

exception UndefinedVariable of string;;

type rec_env = RecEnv of (string -> extGuard partial);;

let emptyRecEnv = RecEnv (fun var -> None);;

let applyRecEnv (RecEnv env) var = match env var with 
    Some g -> g 
  | None -> raise (UndefinedVariable var);;

let bindRecEnv (RecEnv env) var guard = RecEnv (fun v -> if (v = var) then Some guard else env v);;

(*checks if recursion variable var is binded in the environment env*)
let isBind (RecEnv env) var = match env var with 
    Some g -> true
  | None -> false;;

(*computes the inverse reset of g for a list of clocks*)
let invResetList g (TSBReset list) = List.fold_left (fun g x -> invReset g x) g list;; 

(*computes the kind of p in the environment env with the kind inference algorithm in the paper. 
Raises UndefinedVariable if some free rec-variable of p is not binded in env*)
let rec envkindof env p = match p with
    ExtSuccess -> True
  | ExtNil -> False
  | ExtExtChoice(list) -> List.fold_left (fun g x -> Or(g , x)) False (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(And(guard,(invResetList (envkindof env p') reset)))) list)
  | ExtIntChoice(list) ->  let gp = List.fold_left (fun g x -> Or(g , x)) False (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(guard)) list) and 
      ge =  List.fold_left (fun g x -> Or(g , x)) False (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(subtract guard (invResetList (envkindof env p') reset))) list)
    in subtract gp ge
  | ExtCall var -> applyRecEnv env var 
  | ExtRec (var,p') -> let gFix env var p' = 
			 let rec f env var p' g = 
			   let g' = envkindof (bindRecEnv env var g) p' 
			   in if (equivalence g g') then g else f env var p' g'
			 in f env var p' True
		       in gFix env var p'
;;

(*computes the kind of p in the environment env. Raises UndefinedVariable if p contains free rec-variables.
IS PART OF THE API*)
let kindof = envkindof emptyRecEnv;;

(*computes the dual of p in the environment env. Raises UndefinedVariable if some free rec-variable
of p is not binded in env*)
let rec envdualof env p = match p with
    ExtSuccess -> ExtSuccess
  | ExtNil -> ExtNil
  | ExtExtChoice(list) -> ExtIntChoice(List.map (
                  fun (act,(TSBExtGuard guard),reset,p') -> 
		    (act,(TSBExtGuard (And(guard,invResetList(envkindof env p') reset))),reset,envdualof env p'))
		      list)
  | ExtIntChoice(list) -> ExtExtChoice(List.map (
                  fun (act,(TSBExtGuard guard),reset,p') -> (act,(TSBExtGuard guard),reset,envdualof env p'))
		    list)
  | ExtCall var ->  if (isBind env var) then ExtCall var else raise (UndefinedVariable var)
  | ExtRec (var,p') -> let k = envkindof env  (ExtRec (var,p')) in 
		       ExtRec(var,envdualof (bindRecEnv env var k) p');;

(*computes the dual of p in the environment env. Raises UndefinedVariable if p contains free rec-variables.
IS PART OF THE API*)
let dualof = envdualof emptyRecEnv;;

(*conversion of tsb_ext_relation in the corresponding ocaml function*)
let op_of_rel rel = match rel with
    ExtLess -> (<)
  | ExtGreat -> (>)
  | ExtLessEq -> (<=)
  | ExtGreatEq -> (>=)
  | ExtEq -> (==);;

(*checks if a clock evaluation nu satisfy the guard g. nu must be of type: tsb_clock -> float*)
let rec satisfy nu g = match g with
    False -> false
  | True -> true
  | SC(x,rel,d) -> (op_of_rel rel) (nu x) (float_of_int d)
  | DC(x,y,rel,d) -> (op_of_rel rel) ((nu x) -. (nu y)) (float_of_int d)
  | And(g,g') -> (satisfy nu g) && (satisfy nu g')
  | Or(g,g') -> (satisfy nu g) || (satisfy nu g')
  | Not(g) -> not (satisfy nu g);;

(*the starting clock evaluation*)
let nu0 (TSBClock s) = 0.;; 

(*checks if p admits a compliant TST by verifing that nu0 satisfy the kind of p
IS PART OF THE API*)
let admitsCompliant p = let k = kindof p in satisfy nu0 k;;
