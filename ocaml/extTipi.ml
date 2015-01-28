(**
*******************************************************
**                                                   **
**      2) Extended Timed TSB - Extended static      **
**                                                   **
*******************************************************
**)

(*-------------------------------------------------- 
#load "str.cma";;
--------------------------------------------------*)


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

(** #3.1 CONVERT CONTRACT. **)
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
	| x::l -> (clockToString x) ^ "," ^ (resetToString l);;

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
	| And(x, y) -> "(" ^ (extGuardToString x) ^ (
			let temp = extGuardToString y in
			match temp with
			| "True" -> ")"
			| _ -> "," ^ temp ^ ")" 
		)
	| Or(x, y) -> "(" ^ (extGuardToString x) ^ "|" ^ (extGuardToString y) ^ ")"
	| Not(x) -> "NOT (" ^ (extGuardToString x) ^ ")"
	| True -> "True"
	| False -> "False";;

(**)
let rec extChoiceToString extChoice typeChoice =
	(match extChoice with
	| [] -> ""
	| (w, TSBExtGuard x, TSBReset y, z)::l' -> (actionToString w) ^ "{" ^ (extGuardToString x) ^ ";" ^ (resetToString y) ^ "}" ^ typeChoice ^ (extTsbToString z) ^ (extChoiceToString l' typeChoice))
and extTsbToString extTsbContract =
	match extTsbContract with
	| ExtNil -> ""
	| ExtSuccess -> ""
	| ExtIntChoice x -> (extChoiceToString x "+")
	| ExtExtChoice x -> (extChoiceToString x "&")
	| ExtRec (x, y) -> "REC 'x' [" ^ (extTsbToString y) ^ "]"
	| ExtCall x -> "'x'";;


(** #3.2 POSTPROCESSING: STRING MUST BE CLEANED. **)
(**)
let testSearching stringInput regExp =
  try Str.search_forward regExp stringInput 0 with Not_found -> -1;;

(**)
let add_star stringInput = 
	stringInput ^ "*";;

(**)
let rec remove_comma stringInput = 
	let regExp = (Str.regexp "[0-9]+,;") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in 
		let temp = (String.sub matched 0 ((String.length matched) - 2))^";" in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_comma stringUpdated;;

(**)
let rec remove_semicolon stringInput = 
	let regExp = (Str.regexp "{;}") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in 
		let temp = (String.sub matched 0 ((String.length matched) - 2))^"}" in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_semicolon stringUpdated;;

(**)
let rec remove_empty_spaces stringInput = 
	let regExp = (Str.regexp "[a-z]+,}") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in
		let temp = (String.sub matched 0 ((String.length matched) - 2))^"}" in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_empty_spaces stringUpdated;;

(**)
let rec remove_wrong_choices stringInput = 
	let regExp = (Str.regexp "[\\+&][^a-z]") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in
		let temp = (String.sub matched 0 ((String.length matched) - 2))^(String.sub matched ((String.length matched) - 1) 1) in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_wrong_choices stringUpdated;;

(**)
let remove_star stringInput =
	String.sub stringInput 0 ((String.length stringInput) - 1);;

(**)
let postprocessingTsbString stringInput = 
	let s = extTsbToString stringInput in
	let s = add_star s in
	let s = remove_comma s in
	let s = remove_semicolon s in
	let s = remove_empty_spaces s in
	let s = remove_wrong_choices s in
	remove_star s;;