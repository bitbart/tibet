(**
*******************************************************
**                                                   **
**      2) Extended Timed TSB - Extended static      **
**                                                   **
*******************************************************
**)


(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES*) 
open Tipi;;





(** 						SECTION #1								**)
(** EXTGUARD: A GUARD THAT CAN HAVE 'OR'. **)
type tsb_ext_relation = ExtLess | ExtGreat | ExtLessEq | ExtGreatEq | ExtEq;;

type extGuard =   SC of tsb_clock * tsb_ext_relation * int              (*simple constraint: es:  t < 4*) 
                | DC of tsb_clock * tsb_clock * tsb_ext_relation * int  (*diagonal constraint: es: t - x < 4*) 
                | And of extGuard * extGuard 
                | Or of extGuard * extGuard 
                | Not of extGuard
								| True
								| False;;

type tsb_extGuard =  TSBExtGuard of extGuard;;

type extTsb = ExtNil | ExtSuccess |
           ExtIntChoice of (tsb_action * tsb_extGuard * tsb_reset * extTsb) list | 
           ExtExtChoice of (tsb_action *tsb_extGuard * tsb_reset * extTsb) list  |
           ExtRec of string * extTsb |
           ExtCall of string ;;





(** 										SECTION #2												**)
(** CONVERT A CONTRACT INTO A CONTRACT THAT SUPPORTS OR. 	**)
(**)
let toExtRelation tsbRelation = 
	match tsbRelation with
	| Less -> ExtLess
	| Great -> ExtGreat
	| LessEq -> ExtLessEq
	| GreatEq -> ExtGreatEq
	| Eq -> ExtEq;;

(**)
let rec toExtGuard tsbGuard =
	match tsbGuard with 
	| (x, y, z)::l' -> And (SC(x, (toExtRelation y), z), (toExtGuard l')) 
	| [] -> True;;

(**)
let rec toExtChoice tsbIntChoice =
	(match tsbIntChoice with
	| [] -> []
	| (w, TSBGuard x, y, z)::l' -> (w, TSBExtGuard(toExtGuard x), y, (toExtTsb z))::(toExtChoice l'))
and toExtTsb standardTsb = 
	match standardTsb with
	| Nil -> ExtNil
	| Success -> ExtSuccess
	| IntChoice x -> ExtIntChoice (toExtChoice x)
	| ExtChoice x -> ExtExtChoice (toExtChoice x)
	| Rec (x, y) -> ExtRec (x, toExtTsb y)
	| Call x -> ExtCall x;;





(** 										SECTION #3												**)
(** CONVERT A CONTRACT THAT SUPPORTS OR INTO A STRING. 		**)
(**)
let actionToString action =
	match action with
	| TSBAction x -> x;;

(**)
let clockToString clock =
	match clock with
	| TSBClock x -> x;;

(**)
let rec resetToString reset = 
	match reset with
	| [] -> ""
	| x::l -> (clockToString x) ^ " " ^ (resetToString l);;

(**)
let extRelationToString extRelation = 
	match extRelation with
	| ExtLess -> "<"
	| ExtGreat -> ">"
	| ExtLessEq -> "<="
	| ExtGreatEq -> ">="
	| ExtEq -> "=";;

(**)
let rec extGuardToString guard =
	match guard with 
	| SC(x, y, z) -> (clockToString x) ^ (extRelationToString y) ^ (string_of_int z)
	| DC(w, x, y, z) -> (clockToString w) ^ "-" ^ (clockToString x) ^ (extRelationToString y) ^ (string_of_int z)
	| And(x, y) -> (extGuardToString x) ^ "," ^ (extGuardToString y)
	| Or(x, y) -> (extGuardToString x) ^ "|" ^ (extGuardToString y)
	| Not(x) -> "NOT (" ^ (extGuardToString x) ^ ")"
	| True -> ""
	| False -> "";;

(**)
let rec extChoiceToString extChoice typeChoice =
	(match extChoice with
	| [] -> ""
	| (w, TSBExtGuard x, TSBReset y, z)::l' -> (actionToString w) ^ "{" ^ (extGuardToString x) ^ "}" ^ (resetToString y) ^ (extTsbToString z) ^ typeChoice ^ (extChoiceToString l' typeChoice))
and extTsbToString extTsbContract =
	match extTsbContract with
	| ExtNil -> ""
	| ExtSuccess -> "1"
	| ExtIntChoice x -> (extChoiceToString x "+")
(*	| ExtExtChoice x -> (extChoiceToString x "&") *)
	| ExtRec (x, y) -> "REC 'x' [" ^ (extTsbToString y) ^ "]"
	| ExtCall x -> "'x'";;





let p = Rec("x", IntChoice [(TSBAction "a", TSBGuard[], TSBReset[], Success);
                            (TSBAction "b", TSBGuard[(TSBClock "t", Less, 2)], TSBReset[], 
                               ExtChoice [(TSBAction "c", TSBGuard[], TSBReset[], Call "x") ]    )]);;

let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[], Success);
                   (TSBAction "b", TSBGuard[(TSBClock "t", Less, 2)], TSBReset[], 
                        IntChoice[(TSBAction "c", TSBGuard[(TSBClock "t", Great, 2)], TSBReset[],
                           ExtChoice[(TSBAction "a", TSBGuard[], TSBReset[], Success   )]  )]  )];;

let r = IntChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10); (TSBClock "x", Less, 12)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;

let p' = toExtTsb p;;
let q' = toExtTsb q;;
let r' = toExtTsb r;;

extTsbToString p';;
extTsbToString q';;
extTsbToString r';;