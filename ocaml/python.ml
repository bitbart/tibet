(** 
 ********************************************************************************
 **																																						 **
 **				PYTHON (4): Offers functions to use python libraries.     					 **
 **																																						 **
 ********************************************************************************
 **)


(* Inclusion to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINE *)
open Unix;;
open Errors;;
open Tipi;;
open ExtTipi;;
open Tools;;

(** 									SECTION #1								**)
(** GENERAL TOOLS USED BY DIFFERENT FUNCTIONS. 	**)



(** #1.1 PYTHON SOURCE STRINGS **)
(* Command. *)
let python_command_start = "python -c '";;

(* Declarations. *)
let python_command_end = "'";;
let python_context_start = "from python_dbm import Context; c = Context([";;
let python_context_end = "], \"c\"); ";;
let python_guard_end = "; ";;

(* Functions. *)
let python_print = "print ";;
let python_down = ".down(); ";;
let python_invReset_start = ".invReset(";;
let python_invReset_end = "); ";;
let python_subtract = "-";;
let python_equivalence = "==";;


(** #1.2 SYSTEM CALLS **)
(* System call to execute a Unix command. Returns a string with the command output. *)
let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
	 let _ = Unix.close_process (ic, oc) in (Buffer.contents buf);;


(** #1.3 TOOLS **)
(* It returns the name of a clock. *)
let pythonStringOfClock clock = 
	match clock with
	| TSBClock c -> c;;

(* It compares two strings. This is used to eliminate duplicates from the list of declared clocks. *)
let comparatorStrings s1 s2 =
	if(s1 > s2) then 1
	else if (s1 == s2) then 0
	else -1;;

(* It eliminates all duplicates from an sorted list. This is used to create the list of declared clocks. *)
let rec compress clocksList =
	match clocksList with
	| a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;

(* It is the same of cparser, it is used for remove spaces, in order to simplify parsing operations. *)
let rec python_remove_spaces s = 
    if String.compare s "" == 0 then "" else
    let c = String.get s 0 in
    let s' = String.sub s 1 ((String.length s)-1) in
    match c with
    | ' ' -> "" ^ python_remove_spaces s'
    | '\012' -> "" ^ python_remove_spaces s'
    | '\n' -> "" ^ python_remove_spaces s'
    | '\r' -> "" ^ python_remove_spaces s' 
    | '\t' -> "" ^ python_remove_spaces s'
  | _ -> (String.make 1 c) ^ python_remove_spaces s';;





(** 										SECTION #2											**)
(** COPARSER FROM EXTENDED GUARD TO PYTHON DELCARATION. **)
(* It doesn't exists a 'main function' that merges all the results about next functions. *)
(* All OCaml functions that need python libraries, call some of this function and some *)
(* of python strings (expressed in SECTION #1): this calls depend on the python library desidered. *)

(* It takes a guard, then returns a list of its clocks names (without duplicates). *)
let clocksNamesFromGuard guard = 
   let rec clocksNamesFromGuard' inputGuard clocksNames =
     (match inputGuard with
       | SC (c, r, i) -> (pythonStringOfClock c)::clocksNames
       | DC (c, c', r, i) -> (pythonStringOfClock c)::((pythonStringOfClock c')::clocksNames)
       | And (g, g') -> List.append (clocksNamesFromGuard' g clocksNames) (clocksNamesFromGuard' g' clocksNames)
       | Or (g, g') -> List.append (clocksNamesFromGuard' g clocksNames) (clocksNamesFromGuard' g' clocksNames)
       | Not g -> []
       | True -> "t"::clocksNames
       | False -> "f"::clocksNames
     ) in compress (List.sort comparatorStrings (clocksNamesFromGuard' guard []));;

(* It takes a list of clocks names and uses it to create the initial python command, the istruction: 'c = Context c =([x, ..., z]) ...' *)
let pythonContextDeclaration clocksNames =
	let rec pythonContextDeclaration' clocksNames command =
		(
			match clocksNames with
			| h::t -> pythonContextDeclaration' t (command^"\""^h^"\", ")
			| [] -> (String.sub command 0 ((String.length command)-2))^python_context_end
		) in
	pythonContextDeclaration' clocksNames python_context_start;;

(* Returns the string representation of a relation. *)
let pythonStringOfTsbRelation relation = 
	match relation with
	| ExtLess -> "<"
	| ExtGreat -> ">"
	| ExtLessEq -> "<="
	| ExtGreatEq -> ">="
	| ExtEq -> "==";;

(* It takes a guard and returns the string with the python instruction used to declare the guard: 'a = (c.x<10)'. *)
(* Python doesn't know 'True' and 'False' (probably also 'Not'): these values are represented with expressions always true and always false. *)
let pythonGuardFromGuard guard =
  let rec pythonGuardFromGuard' inputGuard clocksNames =
    (match inputGuard with
      | SC (c, r, i) -> "(c." ^ (pythonStringOfClock c) ^ (pythonStringOfTsbRelation r) ^ (string_of_int i) ^ ")"
      | DC (c, c', r, i) -> "(c." ^ (pythonStringOfClock c) ^ " - c." ^ (pythonStringOfClock c') ^ (pythonStringOfTsbRelation r) ^ (string_of_int i) ^ ")"
      | And (g, g') -> "(" ^ (pythonGuardFromGuard' g clocksNames) ^ " & " ^ (pythonGuardFromGuard' g' clocksNames) ^ ")"
      | Or (g, g') -> "(" ^ (pythonGuardFromGuard' g clocksNames) ^ " | " ^ (pythonGuardFromGuard' g' clocksNames) ^ ")"
      | Not g -> ""
      | True -> "(c.t>=0)"
      | False -> "(c.f<0)"
     ) in pythonGuardFromGuard' guard [];;

(* It takes a guardName (string) and a guard, then returns the string with the python instruction used to declare the guard: 'a = (c.x<10)'. *)
let pythonGuardDeclaration guardName guard = 
	guardName ^ "=" ^ (pythonGuardFromGuard guard) ^ python_guard_end;; 





(** 										SECTION #3											**)
(** PARSER FROM PYTHON TO EXTENDED GUARD. PYTHON 				**)
(** RETURNS A STRING THAT REPRESENTS A GUARD:  THE 			**)
(** RESULT MUST BE TRANSLATED IN AN EXTENDED GUARD. 		**)
(** THE FOLLOWING CODE SEMPLIFIES THE PARSING PROCEDURE **)
(** WITH A LONG PREPROCESSING, THEN PARSES RETURNING 		**)
(** THE REQUESTED GUARD.																**)


(** #3.1 PREPROCESSING: REMOVE CONTEXT NAME FROM PYTHON OUTPUT AND DIVIDE CLOCK NAMES. **)
(* Each clock returned by Python is expressed in the form 'c.x' where 'c' is the *)
(* context name and 'x' is the name of the clock. This function removes the *)
(* context name and appends a semicolon to simplify parsing by showing *)
(* clearly the end of a clock name. *)
let rec remove_context stringInput = 
	let regExp = (Str.regexp "c.[a-z]+") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in 
		let temp = (String.sub matched 2 ((String.length matched) - 2))^";" in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_context stringUpdated;;


(** #3.2 REPLACE UNCOMFORTABLE SYMBOLS **)
(* True and False are replaced respectively with  '!' and '?' so the parser can easly distinguish them from the clock names. *)
(* Final python parser will replace '!' and '?' with the types True and False. *)
let replacing_bool stringGuard = 
	let stringUpdated = Str.global_replace (Str.regexp "true") "?" stringGuard in
	Str.global_replace (Str.regexp "false") "!" stringUpdated;;

(* To simplify parsing, all guards with two symbols ('<=', '>=' and '==') are replaced with a guard with only one symbol: respectively '$', '%', and '='. *)
(* Now all guards have only one symbol and are parsed with only one step: ('<', '>', '=', '$', '%', '='). *)
let replacing_equal stringGuard = 
	let stringUpdated = Str.global_replace (Str.regexp "<=") "$" stringGuard in
	let stringUpdated = Str.global_replace (Str.regexp ">=") "%" stringUpdated in
	Str.global_replace (Str.regexp "==") "=" stringUpdated;;

(* Python represents diagonal constraints in two possible forms: 'z<=t' or 'z-t<=5' *)
(* Diagonal constraints in the first form, after preceding steps, becomes 'z;$t;'. *)
(* To semplify the creation of Ocaml diagonal constraints (during parsing), this form becomes 'z;-t;$0'. *)
(* Now the first form is converted to the second form and exists only one form for represent a diagonal constraint. *)
let rec replacing_difference_variables stringGuard =
	let regExp1 = (Str.regexp "[a-z]+;[\\$%=<>][a-z]+;") in														  (* 		(y;<4 & y;<x;) 	*)
	if ((testSearching stringGuard regExp1) == -1) then stringGuard else
		let stringMatched = Str.matched_string stringGuard in															(*		y;<x;						*)
		let regExp2 = (Str.regexp "[\\$%=<>]") in
			if ((testSearching stringMatched regExp2) == -1) then stringGuard else
				let sign = Str.matched_string stringMatched in																(*		<								*)				
				let replace = Str.replace_first regExp2 "-" stringMatched in									(*		y;-x;						*)				
				let replace = replace ^ sign ^ "0" in 																				(*		y;-x;<0					*)
				let stringUpdated = Str.replace_first regExp1 replace stringGuard in					(*		y;<4 & y;-x;<0	*)
				replacing_difference_variables stringUpdated;;

(* The '@' indicates a difference between two guards, in order to use '-' symbol for negative numbers only. *) 
(* All diagonal constraints are (after preceding steps) in the form: 'z;-t;$0'. *)
(* Now diagonal constraints become in the form: 'z;@t;$0'. *)
let rec replacing_minus stringGuard =
	let regExp1 = (Str.regexp "[a-z]+;-[a-z]+") in
	if ((testSearching stringGuard regExp1) == -1) then stringGuard else
		let stringMatched = Str.matched_string stringGuard in
		let regExp2 = (Str.regexp "-") in
		let replace = Str.replace_first regExp2 "@" stringMatched in
		let stringUpdated = Str.replace_first regExp1 replace stringGuard in
		replacing_minus stringUpdated;;

(* Main function to replace uncomfortable symbols in the Python output. *)
let replacing_uncomfortable stringGuard =
	 replacing_minus (replacing_difference_variables (replacing_equal (replacing_bool stringGuard)));;


(** #3.3 INFIX TO PREFIX. **)
(* It shows the priority of python operators. *) 
let python_getOp_prio op =
	match op with
	| '@' -> 6
	| '|' -> 5
	| '&' -> 5
	| ')' -> 2
	| '(' -> 2
	| _ -> 0;;

(* Algorithm for handle the stack. *)
let rec python_update_stack res pr =
	let (s, l) = res in
		match l with
		| c::l' ->
			if (python_getOp_prio c >= pr) then 
				(if (pr==2 && (python_getOp_prio c) == 2) then (s, l') else python_update_stack (s^(String.make 1 c), l') pr)
			else res
		| [] -> res;;

(* Algorithm for conversion from infix to prefix notation *)
let rec python_infix_to_prefix' s stack =
	match s with
	| "" -> 
		let (output, l) = python_update_stack ("", stack) 0 in output
	| _ ->
		let c = String.get s 0 in
		let s' = String.sub s 1 ((String.length s) - 1) in
		match c with
		| ')' -> python_infix_to_prefix' s' (')'::stack)
		| '|' ->
			let (output, new_stack) = python_update_stack ("", stack) (python_getOp_prio '|') in
			output ^ python_infix_to_prefix' s' ('|'::new_stack)
		| '&' ->
			let (output, new_stack) = python_update_stack ("", stack) (python_getOp_prio '&') in
			output ^ python_infix_to_prefix' s' ('&'::new_stack)
		| '(' -> 
			let (output, new_stack) = python_update_stack ("", stack) (python_getOp_prio '(') in
			output ^ python_infix_to_prefix' s' new_stack
		| '@' -> 
			let (output, new_stack) = python_update_stack ("", stack) (python_getOp_prio '@') in
			output ^ python_infix_to_prefix' s' ('@'::new_stack)
	  | _ -> (String.make 1 c) ^ python_infix_to_prefix' s' stack;;

(* Main function to convert from infix to prefix notation. *)
let python_infix_to_prefix s = python_reverse(python_infix_to_prefix' (python_reverse (python_remove_spaces (s))) []);;


(** #3.4 REVERSE GUARD **)
(* Support to reverse guards*)
let reverse_guard_type guardType = 
	match guardType with
	| "<" -> ">"
	| "$" -> "%"
	| "=" -> "="
	| ">" -> "<"
	| _ -> "$";;

(* Python returns the guards in two forms: 'x>5' or '-3<t'. In order to simplify parsing, *)
(* all guards are kept in only one form (the first). So the second form (that after *)
(* preceding steps become '-3<t;') now become 't;>-3'. *)
let rec reverse_guard stringGuard = 
	let regExp = (Str.regexp "[-]*[0-9]+[\\$%=<>][a-z]+;") in
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let stringTest = Str.matched_string stringGuard in
		let regExp = (Str.regexp "[-]*[0-9]+") in
		if ((testSearching stringTest regExp) == -1) then "ERROR" else
			let guardValue = Str.matched_string stringTest in
			let regExp = (Str.regexp "[\\$%=<>]") in
			if ((testSearching stringTest regExp) == -1) then "ERROR" else
				let guardType = Str.matched_string stringTest in
				let regExp = (Str.regexp "[a-z]+;") in
				if ((testSearching stringTest regExp) == -1) then "ERROR" else
					let guardName = Str.matched_string stringTest in
					let regExp = (Str.regexp "[-]*[0-9]+[\\$%=<>][a-z]+;") in
					let newGuard = (guardName ^ (reverse_guard_type guardType) ^ guardValue) in
					let stringUpdated = Str.replace_first regExp newGuard stringGuard in
					reverse_guard stringUpdated;;


(** #3.5 ADDING GUARD SEPARATOR **)
(* In order to semplify parsing, all guards are enclosed by braces. *)
let rec adding_open_guard_separator stringGuard = 
	let regExp = (Str.regexp "[^{]*[@]*[a-z]+") in
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let regExp = (Str.regexp "[@]*[a-z]+") in
		if ((testSearching stringGuard regExp) == -1) then stringGuard else		
		let stringMatched = Str.matched_string stringGuard in
		let stringSubstituted = "{" ^ stringMatched in
		let stringUpdated = Str.replace_first regExp stringSubstituted stringGuard in
		let position = ((String.index stringUpdated '}') + 1) in
		let thisFragment = String.sub stringUpdated 0 position in
		let nextFragment = String.sub stringUpdated position ((String.length stringUpdated)-position) in
		thisFragment^(adding_open_guard_separator nextFragment);;

(* In order to semplify parsing, all guards are enclosed by braces. *)
let rec adding_close_guard_separator stringGuard =
	let regExp = (Str.regexp "[0-9]+[^}]*") in
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let regExp = (Str.regexp "[0-9]+") in
		if ((testSearching stringGuard regExp) == -1) then stringGuard else		
		let stringMatched = Str.matched_string stringGuard in
		let stringSubstituted = stringMatched ^ "}" in
		let stringUpdated = Str.replace_first regExp stringSubstituted stringGuard in
		let position = ((String.index stringUpdated '}') + 1) in
		let thisFragment = String.sub stringUpdated 0 position in
		let nextFragment = String.sub stringUpdated position ((String.length stringUpdated)-position) in
		thisFragment^(adding_close_guard_separator nextFragment);;

(* In order to semplify parsing, all guards are enclosed by braces. *)
let adding_guard_separator stringGuard =
	adding_open_guard_separator (adding_close_guard_separator stringGuard);;


(** #3.6 PYTHON MAIN PARSER **)
(* Guard-value parser. Values are at the end of the guard and guards are enclosed by braces. *)
let rec python_parse_guardValue =
	parser
  [< ''}' >] -> ""
  | [< 'x; y = python_parse_guardValue ?? "Error calculating value" >] -> (String.make 1 x ^ y);;

(* Guard-operator parser. Operators length is always 1 character. *)
let python_parse_guardType =
	parser
	[< ''$' >] -> ExtLessEq					  (*  <=  *)
	| [< ''%' >] -> ExtGreatEq				(*  >=  *)
	| [< ''=' >] -> ExtEq							(*  =  *)
	| [< ''<' >] -> ExtLess					  (*  <  *)
	| [< ''>' >] -> ExtGreat;; 				(*  >  *)

(* Guard-name parser. Names length is arbitrary but a semicolon shows the end of a name. *)
let rec python_parse_name =
	parser
	[< '';' >] -> "" 
	| [< 'x; y = python_parse_name ?? raise (Stream.Error "Missing variable name") >] -> (String.make 1 x ^ y) ;;

(* Main guard-parser. There are two types of guards: diagonal constraint (always marked by a '@') *)
(* and simple constraint. According to the extended types syntax, guards are builded. *)
let python_parse_guards =
	parser
	[< ''@'; 
			w = python_parse_name ?? raise (Stream.Error "Missing diagonal constraint first variable"); 
			x = python_parse_name ?? raise (Stream.Error "Missing diagonal constraint second variable"); 
			y = python_parse_guardType ?? raise (Stream.Error "Missing diagonal constraint operator"); 
			z = python_parse_guardValue ?? raise (Stream.Error "Missing diagonal constraint value") >] -> 
				DC (TSBClock w, TSBClock x, y, (int_of_string z))
	| [< 'w;
			x = python_parse_name ?? raise (Stream.Error "Missing simple constraint second variable"); 
			y = python_parse_guardType ?? raise (Stream.Error "Missing simple constraint operator"); 
			z = python_parse_guardValue ?? raise (Stream.Error "Missing simple constraint value") >] ->
				SC(TSBClock (String.make 1 w ^ x), y, (int_of_string z));;

(* Main guard parser from python output to extended guard. *)
let rec python_parser =
	parser
	[< ''{'; 
			x = python_parse_guards ?? raise (Stream.Error "Missing guard"); >] -> x
	| [< ''?' >] -> True
	| [< ''!' >] -> False
	| [< ''|'; 
			x = python_parser ?? raise (Stream.Error "Missing first OR branch");
			y = python_parser ?? raise (Stream.Error "Missing second OR branch") >] -> Or (x, y)
	| [< ''&'; 
			x = python_parser ?? raise (Stream.Error "Missing first AND branch");
			y = python_parser ?? raise (Stream.Error "Missing second AND branch") >] -> And (x, y);;


(* It takes the output received by python libraries and converts it in a guard. *)
(* 0) Take Python output and remove the final '\n' *)
(* 1) Remove context name and mark all variables with a semicolon. *)
(* 2) Replacing symbols uncomfortable: '<=', '>=', '==', true, false and '-' (for guards only). *)
(* 3) Reverse guards from '4<x' to 'x>4'. *)
(* 4) Infix to Prefix. *)
(* 5) Adding guard separator from 'x>4' to '{x>4}'. *)
(* 6) Parsing result from string to extended guard. *)
let toGuard pythonOutput =
	let guard0 = String.sub pythonOutput 0 ((String.length pythonOutput)-1) in
	let guard1 = remove_context guard0 in
	let guard2 = replacing_uncomfortable guard1 in
	let guard3 = reverse_guard guard2 in
	let guard4 = python_infix_to_prefix guard3 in
	let guard5 = adding_guard_separator guard4 in
	python_parser (Stream.of_string guard5);;





(** 						SECTION #4								**)
(** OCAML INTERFACE TO PYTHON LIBRARIES. 	**)


(** #4.1 PAST: CALLS PYTHON FUNCTION 'DOWN' **)
(* It takes a guard, then calls python libraries and executes the 'down' function. It returns a new guard. *)
let past guard = 
	let clocksNames = clocksNamesFromGuard guard in
	let guardName = "a" in 
	let command = python_command_start^
	  (pythonContextDeclaration clocksNames)^
	  (pythonGuardDeclaration guardName guard)^
	  python_print^
	  guardName^
	  python_down^
	  python_command_end in
  	toGuard (syscall command);;


(** #4.2 INVRESET: CALLS PYTHON FUNCTION 'INVRESET' **)
(* It takes a guard and a clock, then calls python libraries and executes the 'invReset' function. It returns a new guard. *)
let invReset guard clock = 
	let declaredClock = pythonStringOfClock clock in 
	let clocksNames = compress (List.sort comparatorStrings (declaredClock::(clocksNamesFromGuard guard))) in
	let guardName = "a" in 
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName guard)^python_print^guardName^python_invReset_start^"c."^declaredClock^python_invReset_end^python_command_end in 
	toGuard (syscall command);; 


(** #4.3 SUBTRACT: CALLS PYTHON OPERATION '-' **)
(* It takes two guards, then call python operation '-'. It returns a new guard. *)
let subtract guard' guard'' =
	let clocksNames' = clocksNamesFromGuard guard' in
	let clocksNames'' = clocksNamesFromGuard guard'' in
	let guardName' = "a" in
	let guardName'' = "b" in
	let clocksNames = compress (List.sort comparatorStrings (clocksNames'@clocksNames'')) in
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName' guard')^(pythonGuardDeclaration guardName'' guard'')^python_print^guardName'^python_subtract^guardName''^python_command_end in 
	toGuard (syscall command);;


(** #4.4 EQUIVALENCE: CALLS PYTHON OPERATION '==' **)
(* It takes two guards, then call python operation '=='. It returns true if guards are equals, false otherwise. *)
let equivalence guard' guard'' =
	let clocksNames' = clocksNamesFromGuard guard' in
	let clocksNames'' = clocksNamesFromGuard guard'' in
	let guardName' = "a" in
	let guardName'' = "b" in
	let clocksNames = compress (List.sort comparatorStrings (clocksNames'@clocksNames'')) in
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName' guard')^(pythonGuardDeclaration guardName'' guard'')^python_print^guardName'^python_equivalence^guardName''^python_command_end in 
	let result = (syscall command) in
	if ((String.compare result "True\n") == 0) then true
	else if ((String.compare result "False\n") == 0) then false
	else failwith _ERR_201;;