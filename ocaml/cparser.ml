(** 
 ********************************************************************************
 **																																						 **
 **				CPARSER (5): contains a converter from string contracts to XML ones  **
 **																																						 **
 ********************************************************************************
 **)

(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *)
open Errors;;
open FromXML;;


(** PRINTC **)
(* Checks if symbols for action names, clocks etc are in (a-z) range, else raises an exception *)
let printc c =
	let s = String.make 1 c in 
	if (Str.string_match (Str.regexp "[a-z]") s 0) then s 
	else raise (Stream.Error _ERR_001);;

(** PRINTV **)
(* Checks if clock value digits are in (0-9) range, else raises an exception *)
let printv c =
	let s = String.make 1 c in 
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
    [< ''\039'; ''['; x = parse_contract' ?? _ERR_014 >] -> "\">" ^ x 
	| [< 'x; y = parse_recname ?? _ERR_015 >] -> (printc x) ^ y
and parse_call =
	parser
    [< ''\039' >] -> "\" />"
	| [< 'x; y = parse_call ?? _ERR_015 >] -> (printc x) ^ y
and parse_contract' =
	parser
    [< ''?'; x = parse_contract' ?? _ERR_016 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_017) 
			else "\n<extaction id=\"" ^ x ^ "\n</extaction>"
	| [< ''!'; x = parse_contract' ?? _ERR_018 >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error _ERR_017) 
			else "\n<intaction id=\"" ^ x ^ "\n</intaction>"
	| [< ''R'; ''E' ?? _ERR_019; ''C' ?? _ERR_020; ''\039' ?? _ERR_021; x = parse_recname ?? _ERR_022 >] -> "\n<rec name=\"" ^ x ^ "\n</rec>"
	| [< '']'; x = parse_contract' >] -> x
	| [< ''{'; g = parse_guards ?? _ERR_023 >] -> 			
			if ((String.compare "\n<guard id=\"\"" (String.sub g 0 13)) == 0) then raise (Stream.Error _ERR_024) 
			else "\">\n<guards>" ^ g
	| [< ''\039'; x = parse_call >] -> "\n<call name=\"" ^ x
	| [< ''+'; x = parse_contract' ?? _ERR_044; y = parse_contract' ?? _ERR_045 >] -> "\n<intchoice>" ^ x ^ y ^ "\n</intchoice>"
	| [< ''&'; x = parse_contract' ?? _ERR_046; y = parse_contract' ?? _ERR_047 >] -> "\n<extchoice>" ^ x ^ y ^ "\n</extchoice>"
	| [< ''.'; x = parse_contract' ?? _ERR_042; y = parse_contract' ?? _ERR_043 >] -> "\n<sequence>" ^ x ^ y ^ "\n</sequence>"
	| [< 'x; y = parse_contract' ?? _ERR_041 >] -> (printc x) ^ y 	(* The case should not occur *)
;;


(** CHECK BRANCHES **)
(*
	- Checks a list of action branches (all internal branches or all external branches), eg:
	  - the list	["a."; "b{}.?.*"]		generates the exception _ERR_039 using the splitting char '!' because in the internal action 'b{}' no tokens are allowed after sequence symbol '.'.
		- the list	["a{}"; "b"]				returns false using the splitting char '!' because this is a valid sequence of internal actions.

	Input
		- list : list of branches;
		- char : a splitting char.

	Output
		- false : if the contract is correct;
		- exception : otherwise.
*)
let rec find_branches' list char =
		    match list with
    | s::l' ->
        (try 
          let p = (Str.search_forward (Str.regexp "^[a-z]+[]\\.\\+\\&\\{\\)\\*]") s 0) in (find_branches' l' char)
          with Not_found -> match char with
				  | "?" -> failwith (_ERR_039 ^ (Str.global_replace (Str.regexp "[\\*]") "" s))
				  | "!" -> failwith (_ERR_040 ^ (Str.global_replace (Str.regexp "[\\*]") "" s))
				  | _ 	-> failwith (_ERR_041 ^ (Str.global_replace (Str.regexp "[\\*]") "" s)) (* The case should not occur *)
				)
    | [] -> false
;;

(*
	Splits the contract in branches using  a 'splitting char', then analyzes them, eg:
		- the contract 	!a.!b{}.?. 			generates the branches ["a."; "b{}.?.*"]	using the splitting char '!', (where * always indicates the end of a contract).
		- the contract	!a.!b{}.?.			generates the branche [".*"] using the splitting char '?'. 
		- the contract	!a{}.!b					generates the branches ["a{}"; "b"] using the splitting char '!'.
		
	NB. the split function in example '?', creates two different elements: "!a.!b{}." and ".*". Because the next checks are performed considering that these
	are branches about an external action ('?') the first element must be excluded (it is not an external action!). For this reason, the 'if' distinguishes
	the two separated cases: the contract starts with the splitting char, and the contract starts with a different char.
	
	Input
		- contract : a contract to check;
		- char : the splitting char.
		
	Output
		- false : if the contract is correct;
		- exception : otherwise.
*)
let find_branches contract char = 
    if (String.compare (String.sub contract 0 1) char == 0) then find_branches' (Str.split (Str.regexp ("\\" ^ char)) (contract ^ "*")) char
    else find_branches' (List.tl (Str.split (Str.regexp ("\\" ^ char)) (contract ^ "*"))) char;;

(* 
  - Checks if a string contract contains "tailing tokens" after actions '}' and after recursion ']', eg: 
		- !a{}hello 							(the "tail" will be ignored by the parser)
		- REC 'x'[!a.'x']hello		(the "tail" will be ignored by the parser)
		- REC 'x'[!a.'x'hello]		(the "tail" will be ignored by the parser)
	- Checks all internal action branches (after '!') and external action branches (after '?'). 
		- !a{}.!b{}hello
		- ?a{}&?

	Input
		- contract' : a contract to check.
	
	Output
		- false : if the contract is correct;
		- exception : otherwise.
*)
let check_branches contract' = 
    let contract = contract' ^ "*" in
		let flag0 = try let res = Str.search_forward (Str.regexp "'[a-z]+'[^]\\[\\+\\&]") contract 0 in failwith (_ERR_052 ^ (String.sub contract 0 (res+3))) with Not_found -> false in
		let flag1 = try let res = Str.search_forward (Str.regexp "\\}[^\\.\\+\\&\\*\\)]") contract 0 in failwith (_ERR_026 ^ (String.sub contract 0 (res+1))) with Not_found -> false in
		let flag2 = try let res = Str.search_forward (Str.regexp "\\][^\\+\\&\\*\\)\\*]") contract 0 in failwith (_ERR_038 ^ (String.sub contract 0 (res+1))) with Not_found -> false in
    let flag3 = find_branches contract "?" in
    let flag4 = find_branches contract "!" in
    flag0 || flag1 || flag2 || flag3 || flag4;;


(* Add empty braces to action names which haven't got them *)
let add_empty_par s' = 
	let s = s' ^ "*" in
	let res = Str.global_replace (Str.regexp "\\([\\!\\?][a-z]+\\)\\([\\.\\+\\&\\)\\*]+\\)") "\\1{}\\2" s in String.sub res 0 (String.length res - 1);;


(** REMOVE_SPACES **)
let rec remove_spaces' =
	parser
	  [< '' '; x = remove_spaces' >] -> x
	| [< ''*' >] -> ""
	| [< 'x; y = remove_spaces' >] -> String.make 1 x ^ y
;;

let remove_spaces s = remove_spaces' (Stream.of_string (s^"*"));;


(** GET_OPERATOR_PRIORITIES **)
let getOp_prio op =
	match op with
	| '+' -> 5
	| '&' -> 5
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
		(String.make 1 (String.get s new_len)) ^ (reverse (String.sub s 0 new_len))
;;


(** UPDATE_STACK & INFIX_TO_PREFIX **)
(* Algorithm for conversion from infix to prefix notation *)
let rec update_stack res pr =
	let (s, l) = res in
		match l with
		| c::l' ->
			if (getOp_prio c >= pr) then 
				(if (pr==2 && (getOp_prio c) == 2) then (s, l') else update_stack (s^(String.make 1 c), l') pr)
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
		| '&' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '&') in
			output ^ infix_to_prefix' s' ('&'::new_stack)
	  | '.' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '.') in
			output ^ infix_to_prefix' s' ('.'::new_stack)
		| '(' -> let (output, new_stack) = update_stack ("", stack) (getOp_prio '(') in
			output ^ infix_to_prefix' s' new_stack
	  | _ -> (String.make 1 c) ^ infix_to_prefix' s' stack
;;

let infix_to_prefix s = reverse(infix_to_prefix' (reverse (remove_spaces (s))) []);;


(** REMOVE_EMPTIES **)
(* Parsing post-processing: removes empty tags, not allowed in contract XML syntax *)
let rec remove_empties' l =
	match l with
	| s :: s' :: l' -> 
			if ((String.compare s "<guards>" == 0 && String.compare s' "</guards>" == 0)) then remove_empties' l' 
			else if (String.compare s "<resets />" == 0 || String.compare s "<resets/>" == 0 || String.compare s "<guards></guards>" == 0) then remove_empties' (s'::l')
			else if (String.compare s' "<guards>" == 0) then s ^ "\n" ^ remove_empties' (s'::l') 
			else s ^ "\n" ^ s' ^ "\n" ^ remove_empties' l'
	| s :: [] -> s
	| [] -> ""
;;  

let rec remove_empties s = remove_empties' (Str.split (Str.regexp "[\n]+") s);;


(** PREPROCESS_REC **)
(* Parsing pre-processing: force parentheses in REC content to avoid an unwanted behavior in infix_to_prefix conversion *)
let preprocess_rec s = 
	let s' = Str.global_replace (Str.regexp "\\[") "[(" s in
	Str.global_replace (Str.regexp "\\]") ")]" s';;


(** MISSING ANGLE_BRACKET -- CURRENTLY UNUSED **)
(* Parsing pre-processing: it checks if there are some angle bracket missing in the xml contract, obtained when the parser lacks checks. *)
let rec missing_angle_bracket' contract state =
	if ((String.compare contract "" == 0) && (state == 0)) then false
	else if (String.compare contract "" == 0) then true
	else if ((state < 0) || (state > 1)) then true 
	else
		let char = String.sub contract 0 1 in
		let contract' = String.sub contract 1 ((String.length contract) -1) in
		if (String.compare char "<" == 0) then missing_angle_bracket' contract' (state + 1) 
		else if (String.compare char ">" == 0) then missing_angle_bracket' contract' (state - 1)
		else missing_angle_bracket' contract' state;;

let missing_angle_bracket contract =
	if (missing_angle_bracket' contract 0) then failwith _ERR_041
	else false;;


(** MISSING OTHER_BRACKET **)
let rec missing_other_bracket' contract round square =
	match contract with
	| "" -> (if ((round == 0) && (square == 0)) then false
 						else if (round > 0) then failwith _ERR_048
						else if (round < 0) then failwith _ERR_049
						else if (square > 0) then failwith _ERR_050
						else failwith _ERR_051
					)	
	| _ -> 
		let char = String.get contract 0 in
		let contract' = String.sub contract 1 ((String.length contract) -1) in (
			match char with
			| '[' -> missing_other_bracket' contract' round (square+1) 
			| ']' -> missing_other_bracket' contract' round (square-1)
			| '(' -> missing_other_bracket' contract' (round+1) square
			| ')' -> missing_other_bracket' contract' (round-1) square 
			| _ -> missing_other_bracket' contract' round square);;

(* Parsing pre-processing: it checks if there are some round bracket or square bracket missing in the contract. *)
let missing_other_bracket contract = 
	missing_other_bracket' contract 0 0;;


(** PARSE_CONTRACT **)
(* Performs all previous functions in the correct order *)
let parse_contract string_contract' =
	if ((missing_other_bracket string_contract') || (check_branches (remove_spaces string_contract'))) then failwith _ERR_041 else
		let string_contract = (preprocess_rec (remove_spaces string_contract')) in
		let xml_contract = remove_empties ("<contract>" ^ parse_contract' (Stream.of_string (infix_to_prefix (add_empty_par string_contract))) ^ "\n</contract>") in
		(*let error = missing_angle_bracket xml_contract in 
			if error then failwith _ERR_041 else*)
				let correct = checkRecursion (readXmlContract xml_contract) in
					if correct then	try Xml.to_string_fmt (Xml.parse_string (removeNestedTag (xml_contract))) with Xml.Error a -> failwith _ERR_041
					else failwith _ERR_025;;

let rec parse_multiple_contracts' l =
	match l with
	| c::l' -> (parse_contract c) ^ "\n" ^ parse_multiple_contracts' l' 
	| [] -> ""
;;

let parse_multiple_contracts c = parse_multiple_contracts' (Str.split (Str.regexp "[|]+") c);;