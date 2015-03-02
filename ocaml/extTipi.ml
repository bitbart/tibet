(**
****************************************************************
**                                                            **
**      EXTTIPI (3): Extended Timed TSB - Extended static.    **
**                                                            **
****************************************************************
**)


(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES*) 
open Tipi;;
open Tools;;
open Errors;;

(** 	SECTION #1								**)
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

(*************************************)
(*                                   *)
(*      Defining equations          *)
(*                                   *)
(*************************************)

type de =  DENil | DESuccess |
           DEIntChoice of (tsb_action * tsb_extGuard * tsb_reset * string) list | 
           DEExtChoice of (tsb_action *tsb_extGuard * tsb_reset * string) list  
;;

type ide = string;;

type def_eqn_nf = ide * (ide * de) list;;



(******************************************************************)
(*                                                                *)
(** 	     SECTION #2	                                         **)
(** IT CONVERTS FROM STANDARD CONTRACT INTO AN EXTENDED CONTRACT **)
(* It Converts from tsb relation to extended tsb relation.        *)
(*                                                                *)
(******************************************************************)
let toExtRelation tsbRelation = 
	match tsbRelation with
	| Less -> ExtLess
	| Great -> ExtGreat
	| LessEq -> ExtLessEq
	| GreatEq -> ExtGreatEq
	| Eq -> ExtEq;;

(* It Converts from tsb guard to extended tsb guard (a guard that supports OR). *)
let rec toExtGuard tsbGuard =
	match tsbGuard with 
	| (x, y, z)::l' -> And (SC(x, (toExtRelation y), z), (toExtGuard l')) 
	| [] -> True;;

(* It Converts from tsb choice to extended tsb choice. *)
let rec toExtChoice tsbIntChoice =
	(match tsbIntChoice with
	| [] -> []
	| (w, TSBGuard x, y, z)::l' -> (w, TSBExtGuard(toExtGuard x), y, (toExtTsb z))::(toExtChoice l'))

(* Main function that converts from tsb contract to extended tsb contract. *)
and toExtTsb standardTsb = 
	match standardTsb with
	| Nil -> ExtNil
	| Success -> ExtSuccess
	| IntChoice x -> ExtIntChoice (toExtChoice x)
	| ExtChoice x -> ExtExtChoice (toExtChoice x)
	| Rec (x, y) -> ExtRec (x, toExtTsb y)
	| Call x -> ExtCall x;;


(**	SECTION #3     **)
(** CONVERT A CONTRACT THAT SUPPORTS OR INTO A STRING. 		**)


(** #3.1 CONTRACT CONVERTER. **)
(* Given an action, it returns the action name. *)
let actionToString action =
	match action with
	| TSBAction x -> x;;

(* Given a clock, it returns the action name. *)
let clockToString clock =
	match clock with
	| TSBClock x -> x;;

(* Given a reset, it returns a string with the list of clocks names. *)
let rec resetToString reset = 
	match reset with
	| [] -> ""
	| x::l -> (clockToString x) ^ (
		let temp = resetToString l in
		match temp with
		| "" -> ""
		| _ -> "," ^ temp);;

(* It returns a string that represent an extended relation. *)
let extRelationToString extRelation = 
	match extRelation with
	| ExtLess -> " < "
	| ExtGreat -> " > "
	| ExtLessEq -> " <= "
	| ExtGreatEq -> " >= "
	| ExtEq -> " == ";;

(* It returns a string that represent an extended guard. *)
let rec extGuardToString guard =
	match guard with 
	| SC(x, y, z) -> (clockToString x) ^ (extRelationToString y) ^ (string_of_int z)
	| DC(w, x, y, z) -> (clockToString w) ^ " - " ^ (clockToString x) ^ (extRelationToString y) ^ (string_of_int z)
	| And(x, y) -> "(" ^ (extGuardToString x) ^ (
			let temp = extGuardToString y in
			match temp with
			| "True" -> ")"
			| _ -> " && " ^ temp ^ ")" 
		)
	| Or(x, y) -> "(" ^ (extGuardToString x) ^ " || " ^ (extGuardToString y) ^ ")"
	| Not(x) -> "!(" ^ (extGuardToString x) ^ ")"
	| True ->  " true "
	| False -> " false ";;

(* It returns a string that represent an extended choice. *)
let rec extChoiceToString extChoice typeChoice =
	(match extChoice with
	| [] -> ""
	| (w, TSBExtGuard x, TSBReset y, z)::l' -> (actionToString w) ^ "{" ^ (extGuardToString x) ^ ";" ^ (resetToString y) ^ "}" ^ typeChoice ^ (extTsbToString' z) ^ (extChoiceToString l' typeChoice))

(* Function that converts from extended tsb contract to a string: result must be postprocessed. *)
and extTsbToString' extTsbContract =
	match extTsbContract with
	| ExtNil -> ""
	| ExtSuccess -> ""
	| ExtIntChoice x -> (extChoiceToString x "+")
	| ExtExtChoice x -> (extChoiceToString x "&")
	| ExtRec (x, y) -> "REC 'x' [" ^ (extTsbToString' y) ^ "]"
	| ExtCall x -> "'x'";;


(** #3.2 POSTPROCESSING: STRING MUST BE CLEANED. **)
(* It adds a special character to support search with regular expression. *)
let add_star stringInput = 
	stringInput ^ "*";;

(* It removes choices symbols that appear at the end of the string. *)
let rec remove_wrong_choices stringInput = 
	let regExp = (Str.regexp "[\\+&][^a-z\\']") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in
		let temp = (String.sub matched 0 ((String.length matched) - 2))^(String.sub matched ((String.length matched) - 1) 1) in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_wrong_choices stringUpdated;;

(* It removes the special character added at the end of the string. *)
let remove_star stringInput =
	String.sub stringInput 0 ((String.length stringInput) - 1);;

(* Main function to perform the translation from extended tsb to string, and then to postprocess the result. *)
let extTsbToString stringInput = 
	let s = extTsbToString' stringInput in
	let s = add_star s in
	let s = remove_wrong_choices s in
	remove_star s;;