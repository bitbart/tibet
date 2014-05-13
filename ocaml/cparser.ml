(** 
 ********************************************************************************
 **																																						 **
 **				CPARSER (5): contains a converter from string contracts to XML ones  **
 **																																						 **
 ********************************************************************************
 **)

(* When compiling with makefile, use ocamlopt -c -pp camlp4o cparser.ml to activate Camlp4 Preprocessing *)

(* Inclusions to be used when compiling with Ocaml Interactive Environment *)
(* 
#load "dynlink.cma";;
#load "camlp4o.cma";;
 *)

(* LIST OF ERRORS *)
let _ERR_001 = "ERR_001: Invalid format for at least one action id or rec id  or clock id. Only a-z allowed.";;
let _ERR_002 = "ERR_002: Invalid format for at least one clock value. Only integer values allowed.";;
let _ERR_003 = "ERR_003: Missing a valid clock id after symbol ',' in at least one reset field.";;
let _ERR_004 = "ERR_004: Missing '}' or ',' in at least one reset field.";;
let _ERR_005 = "ERR_005: Missing a valid guard after symbol ',' in at least one guard field.";;
let _ERR_006 = "ERR_006: Missing a valid guard after symbol ',' in at least one guard field.";;
let _ERR_007 = "ERR_007: Missing a valid clock id after symbol ';' in at least one reset field.";;
let _ERR_008 = "ERR_008: Missing '}' or ';' after a guard.";;
let _ERR_009 = "ERR_009: Missing a valid clock value after symbol '<' in at least one guard field.";;
let _ERR_010 = "ERR_010: Missing a valid clock value after symbol '>' in at least one guard field.";;
let _ERR_011 = "ERR_011: Missing a valid guard in at least one guard field.";;
let _ERR_012 = "ERR_012: Missing a valid clock id after symbol ';' in at least one reset field.";;
let _ERR_013 = "ERR_013: Missing a valid reset in at least one reset field.";;
let _ERR_014 = "ERR_014: Missing a valid sequence or choice inside a REC[ ] declaration.";;
let _ERR_015 = "ERR_015: Missing '[' after at least one REC name.";;
let _ERR_016 = "ERR_016: Missing a valid action name after symbol '?'.";;
let _ERR_017 = "ERR_017: An action cannot have an empty identifier!";;
let _ERR_018 = "ERR_018: Missing a valid action name after symbol '!'"
let _ERR_019 = "ERR_019: Missing 'E' after 'R' in REC declaration.";;
let _ERR_020 = "ERR_020: Missing 'C' after 'E' in REC declaration.";;
let _ERR_021 = "ERR_021: Missing '_' after 'C' in REC declaration.";;
let _ERR_022 = "ERR_022: Missing variable name in REC." ;;
let _ERR_023 = "ERR_023: Error parsing guard after '{' symbol." ;;
let _ERR_024 = "ERR_024: A clock id cannot have an empty identifier!";;

(** PRINTC **)
(* Checks if symbols for action names, clocks etc are in (a-z) range, else raises an exception *)
let printc c =
	let s = Char.escaped c in 
	if (Str.string_match (Str.regexp "[a-z]") s 0) then s 
	else raise (Stream.Error _ERR_001);;

(** PRINTV **)
(* Checks if clock value digits are in (0-9) range, else raises an exception *)
let printv c =
	let s = Char.escaped c in 
	if (Str.string_match (Str.regexp "[0-9]") s 0) then s 
	else raise (Stream.Error _ERR_002);;


(** MAIN PARSER **)
let rec parse_resets =
		parser
		[< '',' ; x = parse_resets ?? _ERR_003 >] -> "\" />\n<reset id=\"" ^ x
	| [< ''}' >] -> "\" />\n</resets>"
	| [< 'x; y = parse_resets ?? _ERR_004 >] -> (printc x) ^ y
and parse_guardValue =
	parser
	  [< '',' ; x = parse_guardType ?? _ERR_005 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_006) 
			else "\" />\n<guard id=\"" ^ x
	| [< ''}' >] -> "\" />\n</guards>"	
	| [< '';'; x = parse_resets ?? _ERR_007 >] -> "\" />\n</guards>\n<resets>\n<reset id=\"" ^ x
	| [< 'x; y = parse_guardValue ?? _ERR_008 >] -> (printv x) ^ y
and parse_guardType =
	parser
	  [< ''<' ; x = parse_guardValue ?? _ERR_008  >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_009) 
			else "\" op=\"less\" value=\"" ^ x
	| [< ''>' ; x = parse_guardValue ?? _ERR_008 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_010) 
			else "\" op=\"great\" value=\"" ^ x
	| [< 'x; y = parse_guardType ?? _ERR_011 >] -> (printc x) ^ y
and parse_guards' =
	parser
		[< '',' >] -> raise (Stream.Error _ERR_012)
	| [< ''}' >] -> "\n</guards>\n<resets />"
	| [< x = parse_resets >] -> "</guards>\n<resets><reset id=\"" ^ x
and parse_guards =
	parser
	  [< ''}' >] -> "\n</guards>\n<resets />"
	| [< '';' ; x = parse_guards' ?? _ERR_013 >] -> x
	| [< x = parse_guardType >] -> "\n<guard id=\"" ^ x 
and parse_recname =
	parser
    [< ''"'; ''['; x = parse_contract' ?? _ERR_014 >] -> "\">" ^ x 
	| [< 'x; y = parse_recname ?? _ERR_015 >] -> (printc x) ^ y
and parse_call =
	parser
    [< ''"' >] -> "\" />"
	| [< 'x; y = parse_call ?? _ERR_015 >] -> (printc x) ^ y
and parse_contract' =
	parser
    [< ''?'; x = parse_contract' ?? _ERR_016 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_017) 
			else "\n<extaction id=\"" ^ x ^ "\n</extaction>"
	| [< ''!'; x = parse_contract' ?? _ERR_018 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_017) 
			else "\n<intaction id=\"" ^ x ^ "\n</intaction>"
	| [< ''R'; ''E' ?? _ERR_019; ''C' ?? _ERR_020; ''"' ?? _ERR_021; x = parse_recname ?? _ERR_022 >] -> "\n<rec name=\"" ^ x ^ "\n</rec>"
	| [< '']'; x = parse_contract' >] -> x
	| [< ''{'; g = parse_guards ?? _ERR_023 >] -> 			
			if ((String.compare "\n<guard id=\"\"" (String.sub g 0 13)) == 0) then raise (Stream.Error _ERR_024) 
			else "\">\n<guards>" ^ g
	| [< ''"'; x = parse_call >] -> "\n<call name=\"" ^ x
	| [< ''#'; x = parse_contract' ?? "1"; y = parse_contract' ?? "7" >] -> "\n<intchoice>" ^ x ^ y ^ "\n</intchoice>"
	| [< ''+'; x = parse_contract' ?? "2"; y = parse_contract' ?? "6" >] -> "\n<extchoice>" ^ x ^ y ^ "\n</extchoice>"
	| [< ''.'; x = parse_contract' ?? "3"; y = parse_contract' ?? "5" >] -> "\n<sequence>" ^ x ^ y ^ "\n</sequence>"
	| [< 'x; y = parse_contract' ?? "4" >] -> (printc x) ^ y
;;

(** REMOVE_SPACES **)
let rec remove_spaces' =
	parser
	  [< '' '; x = remove_spaces' >] -> x
	| [< ''*' >] -> ""
	| [< 'x; y = remove_spaces' >] -> Char.escaped x ^ y
;;

let remove_spaces s = remove_spaces' (Stream.of_string (s^"*"));;

(** GET_OPERATOR_PRIORITIES **)
let getOp_prio op =
	match op with
	| '+' -> 5
	| '#' -> 5
	| '.' -> 6
	| ')' -> 2
	| '(' -> 2
	| _ -> 0
;;

(** REVERSE string **)
let rec reverse s =
	match s with
	| "" -> ""
	| _ ->
		let new_len =  (String.length s) - 1 in
		(Char.escaped (String.get s new_len)) ^ (reverse (String.sub s 0 new_len))
;;

let rec update_stack res pr =
	let (s, l) = res in
		match l with
		| c::l' ->
			if (getOp_prio c >= pr) then 
				(if (pr==2 && (getOp_prio c) == 2) then (s, l') else update_stack (s^(Char.escaped c), l') pr)
			else res
		| [] -> res
;; 

let rec infix_to_prefix' s stack =
	match s with
	| "" -> 
		let (output, l) = update_stack ("", stack) 0 in output
	| _ ->
		let c = String.get s 0 in
		let s' = String.sub s 1 ((String.length s) - 1) in
		match c with
		| ')' -> infix_to_prefix' s' (')'::stack)
		| '+' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '+') in
			output ^ infix_to_prefix' s' ('+'::new_stack)
		| '#' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '#') in
			output ^ infix_to_prefix' s' ('#'::new_stack)
	  | '.' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '.') in
			output ^ infix_to_prefix' s' ('.'::new_stack)
		| '(' -> let (output, new_stack) = update_stack ("", stack) (getOp_prio '(') in
			output ^ infix_to_prefix' s' new_stack
	  | _ -> (Char.escaped c) ^ infix_to_prefix' s' stack
;;

let infix_to_prefix s = reverse(infix_to_prefix' (reverse (remove_spaces (s))) []);;

let rec remove_empties' l =
	match l with
	| s :: s' :: l' -> 
			if (String.compare s "<guards>" == 0 && String.compare s' "</guards>" == 0) then remove_empties' l' 
			else if (String.compare s "<resets />" == 0 || String.compare s "<resets/>" == 0) then remove_empties' (s'::l')
			else if (String.compare s' "<guards>" == 0) then s ^ "\n" ^ remove_empties' (s'::l') 
			else s ^ "\n" ^ s' ^ "\n" ^ remove_empties' l'
	| s :: [] -> s
	| [] -> ""
;;  

let preprocess_rec s = 
	let s' = Str.global_replace (Str.regexp "\\[") "[(" s in
	Str.global_replace (Str.regexp "\\]") ")]" s';;

let rec remove_empties s = remove_empties' (Str.split (Str.regexp "[\n]+") s);;

let parse_contract c = remove_empties ("<contract>" ^ parse_contract' (Stream.of_string (infix_to_prefix (preprocess_rec c))) ^ "\n</contract>");;

let rec parse_multiple_contracts' l =
	match l with
	| c::l' -> (parse_contract c) ^ "\n" ^ parse_multiple_contracts' l' 
	| [] -> ""
;;

let parse_multiple_contracts c = parse_multiple_contracts' (Str.split (Str.regexp "[|]+") c);;
