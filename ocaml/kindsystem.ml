(** ************************************************************************** **)
(**                                                                            **)
(**  	KINDSYSTEM (5): Offers functions to calculate the dual of a contract     **)
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

(*Da cancellare
let invReset (g:extGuard) (TSBClock x) = True;;
let past (g:extGuard) = True;;
let subtract (g:extGuard) (g':extGuard) = False;;
let equiv (g:extGuard) (g':extGuard) = true;;*)

type 'a partial = Some of 'a | None ;;

exception UndefinedVariable of string;;

type rec_env = RecEnv of (string -> extGuard partial);;

let emptyRecEnv = RecEnv (fun var -> None);;

let applyRecEnv (RecEnv env) var = match env var with 
    Some g -> g 
  | None -> raise (UndefinedVariable var);;

let bindRecEnv (RecEnv env) var guard = RecEnv (fun v -> if (v = var) then Some guard else env v);;


let invResetList (g:extGuard) (TSBReset list) = List.fold_left (fun g x -> Or(g , invReset g x)) True list;; 

let rec kindof env p = match p with
    ExtSuccess -> True
  | ExtNil -> False
  | ExtExtChoice(list) -> List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(And(guard,(invResetList (kindof env p') reset)))) list)
  | ExtIntChoice(list) ->  let gp = List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(guard)) list) and 
      ge =  List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(subtract guard (invResetList (kindof env p') reset))) list)
    in subtract gp ge
  | ExtCall var -> applyRecEnv env var 
  | ExtRec (var,p') -> let gFix env var p' = 
			 let rec f env var p' g = 
			   let g' = kindof (bindRecEnv env var g) p' 
			   in if (equivalence g g') then g else f env var p' g'
			 in f env var p' True
		       in gFix env var p'
;;

let kindof p = kindof emptyRecEnv p;;


(*
let g1 = (And(SC(TSBClock "x", ExtLess, 4),DC (TSBClock "x", TSBClock "t", ExtLessEq, 7)));;
let g2 = (Or(SC(TSBClock "x", ExtEq, 4),DC (TSBClock "x", TSBClock "t", ExtGreatEq, 7)));;
let g3 = (Or(SC(TSBClock "t", ExtEq, 4), Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));;
let g4 = (Or(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g5 = (And(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g6 = (Or(And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g7 = (Or(SC(TSBClock "t", ExtEq, 4), And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));;
let gA = (Or(SC(TSBClock "t", ExtEq , 4), And(SC(TSBClock "x", ExtEq, 5), SC (TSBClock "s", ExtEq, 6))));;
let gB = (And(Or(SC(TSBClock "t", ExtEq , 4),SC(TSBClock "x", ExtEq, 5)),  SC (TSBClock "s", ExtEq, 6)));;

past g1;;
past g2;;
past g3;;
past g4;;
past g5;;
past g6;;
past g7;;
past gA;;
past gB;;

let clockX = TSBClock "x";;

invReset g1 clockX;;
invReset g2 clockX;;
invReset g3 clockX;;
invReset g4 clockX;;
invReset g5 clockX;;
invReset g6 clockX;;
invReset g7 clockX;;
invReset gA clockX;;
invReset gB clockX;;

subtract g1 g2;;
subtract g1 g3;;
subtract g1 g4;;
subtract g1 g5;;
subtract g1 g6;;
subtract g1 g7;;
subtract g1 gA;;
subtract g1 gB;;

equivalence g1 g1;;
equivalence g1 g2;;
equivalence g1 g3;;
equivalence g1 g4;;
equivalence g1 g5;;
equivalence g1 g6;;
equivalence g1 g7;;
equivalence g1 gA;;
equivalence g1 gB;;*)
