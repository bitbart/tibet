(** 
 ********************************************************************************
 **																																						 **
 **				PYTHON (4): Offers functions to use python libraries      					 **
 **																																						 **
 ********************************************************************************
 **)

(*-------------------------------------------------- 
   OCAML TOPLEVEL IMPORTS (for Eclipse )
	#load "unix.cma"
--------------------------------------------------*)

(* Inclusion to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINE *)
open Unix;;
open Errors;;
open Tipi;;
open ExtTipi;;

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

(* It is used to substitute the '-' used to indicate a Diagonal Constraint: (eg. x-y<10). *)
(* In this way '-' it is not confused with '-' used for negative numbers. *)
let substitute_minus_char char =
	match char with
	| '-' -> '@'
	| _ -> char;;

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

(* It looks for a regular expression in a string: if there is no match, function returns -1, else it returns the position of the first result in the string. *)
let testSearching stringInput regExp =
  try Str.search_forward regExp stringInput 0 with Not_found -> -1;;





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





(** 						SECTION #3										**)
(** PARSER FROM PYTHON DELCARATION TO GUARD. 	**)


(** #3.1 PREPROCESSING: REMOVE CONTEXT NAME FROM PYTHON OUTPUT AND DIVIDE CLOCK NAMES. **)
(**)
let rec remove_context stringInput = 
	let regExp = (Str.regexp "c.[a-z]+") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in 
		let temp = (String.sub matched 2 ((String.length matched) - 2))^";" in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_context stringUpdated;;


(** #3.2 REPLACE UNCOMFORTABLE SYMBOLS **)
(**)
let replacing_bool stringGuard = 
	let stringUpdated = Str.global_replace (Str.regexp "true") "?" stringGuard in
	Str.global_replace (Str.regexp "false") "!" stringUpdated;;

(**)
let replacing_equal stringGuard = 
	let stringUpdated = Str.global_replace (Str.regexp "<=") "$" stringGuard in
	let stringUpdated = Str.global_replace (Str.regexp ">=") "%" stringUpdated in
	Str.global_replace (Str.regexp "==") "=" stringUpdated;;

(**)
let rec replacing_difference_variables stringGuard =
	let regExp1 = (Str.regexp "[a-z]+;[\\$%=<>][a-z]+;") in															(* 		(y;<4 & y;<x;) 	*)
	if ((testSearching stringGuard regExp1) == -1) then stringGuard else
		let stringMatched = Str.matched_string stringGuard in															(*		y;<x;						*)
		let regExp2 = (Str.regexp "[\\$%=<>]") in
			if ((testSearching stringMatched regExp2) == -1) then stringGuard else
				let sign = Str.matched_string stringMatched in																(*		<								*)				
				let replace = Str.replace_first regExp2 "-" stringMatched in									(*		y;-x;						*)				
				let replace = replace ^ sign ^ "0" in 																				(*		y;-x;<0					*)
				let stringUpdated = Str.replace_first regExp1 replace stringGuard in					(*		y;<4 & y;-x;<0	*)
				replacing_difference_variables stringUpdated;;

(**)
let rec replacing_minus stringGuard =
	let regExp = (Str.regexp "[a-z]+;-[a-z]+") in
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let stringMatched = Str.matched_string stringGuard in
		let stringSubstituted = String.map substitute_minus_char stringMatched in
		let stringUpdated = Str.replace_first regExp stringSubstituted stringGuard in
		replacing_minus stringUpdated;;

(**)
let replacing_uncomfortable stringGuard =
	replacing_minus (replacing_difference_variables (replacing_equal (replacing_bool stringGuard)));;


(** #3.3 INFIX TO PREFIX. **)

(**)
let rec python_reverse s =
	match s with
	| "" -> ""
	| _ ->
		let new_len =  (String.length s) - 1 in
		(String.make 1 (String.get s new_len)) ^ (python_reverse (String.sub s 0 new_len));;

(**) 
let python_getOp_prio op =
	match op with
	| '@' -> 6
	| '|' -> 5
	| '&' -> 5
	| ')' -> 2
	| '(' -> 2
	| _ -> 0;;

(* Algorithm for conversion from infix to prefix notation *)
let rec python_update_stack res pr =
	let (s, l) = res in
		match l with
		| c::l' ->
			if (python_getOp_prio c >= pr) then 
				(if (pr==2 && (python_getOp_prio c) == 2) then (s, l') else python_update_stack (s^(String.make 1 c), l') pr)
			else res
		| [] -> res;;

(**)
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

(**)
let python_infix_to_prefix s = python_reverse(python_infix_to_prefix' (python_reverse (python_remove_spaces (s))) []);;


(** #3.4 REVERSE GUARD **)
let reverse_guard_type guardType = 
	match guardType with
	| "<" -> ">"
	| "$" -> "%"
	| "=" -> "="
	| ">" -> "<"
	| _ -> "$";;

(**)
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
(**)
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

(**)
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

(**)
let adding_guard_separator stringGuard =
	adding_open_guard_separator (adding_close_guard_separator stringGuard);;


(** #3.6 PYTHON MAIN PARSER **)
(**)
let rec python_parse_guardValue =
	parser
  [< ''}' >] -> ""
  | [< 'x; y = python_parse_guardValue ?? "Error calculating value" >] -> (String.make 1 x ^ y);;

(**)
let python_parse_guardType =
	parser
	[< ''$' >] -> ExtLessEq					  (*  <=  *)
	| [< ''%' >] -> ExtGreatEq				(*  >=  *)
	| [< ''=' >] -> ExtEq							(*  =  *)
	| [< ''<' >] -> ExtLess					  (*  <  *)
	| [< ''>' >] -> ExtGreat;; 				(*  >  *)

(**)
let rec python_parse_name =
	parser
	[< '';' >] -> "" 
	| [< 'x; y = python_parse_name ?? raise (Stream.Error "Missing variable name") >] -> (String.make 1 x ^ y) ;;

(**)
let python_parse_guards =
	parser
	[< ''@'; 
			w = python_parse_name ?? raise (Stream.Error "Missing diagonal first variable"); 
			x = python_parse_name ?? raise (Stream.Error "Missing diagonal second variable"); 
			y = python_parse_guardType ?? raise (Stream.Error "Missing diagonal operator"); 
			z = python_parse_guardValue ?? raise (Stream.Error "Missing diagonal value") >] -> 
				DC (TSBClock w, TSBClock x, y, (int_of_string z))
	| [< 'w;
			x = python_parse_name ?? raise (Stream.Error "Missing diagonal second variable"); 
			y = python_parse_guardType ?? raise (Stream.Error "Missing diagonal operator"); 
			z = python_parse_guardValue ?? raise (Stream.Error "Missing diagonal value") >] ->
				SC(TSBClock (String.make 1 w ^ x), y, (int_of_string z));;

(**)
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
(* 0) Take Python output. *)
(* 1) Remove the final '\n' *)
(* 1) Remove context name. *)
(* 2) Replacing symbols uncomfortable: '<=' became '$', '>=' became '%' and '==' became '='. *)
(* 3) Infix to Prefix. *)
(* 4) Reverse guards. *)
(* 5) Adding guard separator. *)
(* 6) Parsing result. *)
let toGuard pythonOutput =
	let guard = String.sub pythonOutput 0 ((String.length pythonOutput)-1) in
	let guard = remove_context guard in
	let guard = replacing_uncomfortable guard in
	let guard = reverse_guard guard in
	let guard = python_infix_to_prefix guard in
	let guard = adding_guard_separator guard in
	python_parser (Stream.of_string guard);;





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
