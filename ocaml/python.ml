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


(** #1.3 STRING TOOLS **)
(* It returns the name of a clock. *)
let pythonStringOfClock clock = 
	match clock with
	| TSBClock c -> c;;

(* If 'list' contains 'element' then returns true else false. This is used to verify when the argument of invReset is a declared clock. *)
let rec exists element list = match list with
  | [] -> false
  | h::t -> if ((String.compare element  h) == 0) then true 
    else exists element t;;

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





(** 										SECTION #2											**)
(** COPARSER FROM EXTENDED GUARD TO PYTHON DELCARATION. **)

(* It takes a guard, then returns a list of its clocks names (without duplicates). *)
let clocksNamesFromGuard guard = 
	match guard with
		| TSBExtGuard inputGuard ->
			let rec clocksNamesFromGuard' inputGuard clocksNames =
			(match inputGuard with
				| SC (c, r, i) -> (pythonStringOfClock c)::clocksNames
				| DC (c, c', r, i) -> (pythonStringOfClock c)::((pythonStringOfClock c')::clocksNames)
				| And (g, g') -> List.append (clocksNamesFromGuard' g clocksNames) (clocksNamesFromGuard' g' clocksNames)
				| Or (g, g') -> List.append (clocksNamesFromGuard' g clocksNames) (clocksNamesFromGuard' g' clocksNames)
				| Not g -> []
				| True -> []
				| False -> []
			) in compress (List.sort comparatorStrings (clocksNamesFromGuard' inputGuard []));;

(* It takes a list of clocks names and uses it to create the initial python command, that is the istruction: 'c = Context c =([...])' *)
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
let pythonGuardFromGuard guard =
		match guard with
		| TSBExtGuard inputGuard ->
			let rec pythonGuardFromGuard' inputGuard clocksNames =
			(match inputGuard with
				| SC (c, r, i) -> "(c." ^ (pythonStringOfClock c) ^ (pythonStringOfTsbRelation r) ^ (string_of_int i) ^ ")"
				| DC (c, c', r, i) -> "(c." ^ (pythonStringOfClock c) ^ " - c." ^ (pythonStringOfClock c') ^ (pythonStringOfTsbRelation r) ^ (string_of_int i) ^ ")"
				| And (g, g') -> "(" ^ (pythonGuardFromGuard' g clocksNames) ^ " & " ^ (pythonGuardFromGuard' g' clocksNames) ^ ")"
				| Or (g, g') -> "(" ^ (pythonGuardFromGuard' g clocksNames) ^ " | " ^ (pythonGuardFromGuard' g' clocksNames) ^ ")"
				| Not g -> ""
				| True -> ""
				| False -> ""
			) in pythonGuardFromGuard' inputGuard [];;

(* It takes a guardName (string) and a guard, then returns the string with the python instruction used to declare the guard: 'a = (c.x<10)'. *)
let pythonGuardDeclaration guardName guard = 
	guardName ^ "=" ^ (pythonGuardFromGuard guard) ^ python_guard_end;; 





(** 						SECTION #3										**)
(** PARSER FROM PYTHON DELCARATION TO GUARD. 	**)

(* It takes the output received by python libraries and converts it in a guard. *)
let toGuard pythonOutput =
	pythonOutput;;


(** #3.1 PREPROCESSING: REMOVE CONTEXT NAME FROM PYTHON OUTPUT. **)
let testSearching stringInput regExp =
  try Str.search_forward regExp stringInput 0 with Not_found -> -1;;

let rec remove_context stringInput = 
	let regExp = (Str.regexp "c.[a-z]+") in
	if ((testSearching stringInput regExp) == -1) then stringInput else
		let matched = (Str.matched_string stringInput) in 
		let temp = String.sub matched 2 ((String.length matched) - 2) in 
		let stringUpdated = Str.replace_first regExp temp stringInput in
		remove_context stringUpdated;;


(** #3.2 INFIX TO PREFIX. **)
let python_getOp_prio op =
	match op with
	| '|' -> 5
	| '&' -> 5
	| ')' -> 2
	| '(' -> 2
	| _ -> 0
;;

let rec python_infix_to_prefix' s stack =
	match s with
	| "" -> 
		let (output, l) = update_stack ("", stack) 0 in output
	| _ ->
		let c = String.get s 0 in
		let s' = String.sub s 1 ((String.length s) - 1) in
		match c with
		| ')' -> python_infix_to_prefix' s' (')'::stack)
		| '|' ->
			let (output, new_stack) = update_stack ("", stack) (python_getOp_prio '|') in
			output ^ python_infix_to_prefix' s' ('|'::new_stack)
		| '&' ->
			let (output, new_stack) = update_stack ("", stack) (python_getOp_prio '&') in
			output ^ python_infix_to_prefix' s' ('&'::new_stack)
		| '(' -> let (output, new_stack) = update_stack ("", stack) (python_getOp_prio '(') in
			output ^ python_infix_to_prefix' s' new_stack
	  | _ -> (String.make 1 c) ^ python_infix_to_prefix' s' stack
;;

let python_infix_to_prefix s = reverse(python_infix_to_prefix' (reverse (remove_spaces (s))) []);;


(** #3.3 REVERSE GUARD **)
let reverse_guard_type guardType = 
	match guardType with
	| "<" -> ">"
	| "<=" -> ">="
	| "==" -> "=="
	| ">" -> "<"
	| ">=" -> "<="
	| _ -> "ERROR";; (* The case should not occur. *)

let rec reverse_guard stringGuard = 
	let regExp = (Str.regexp "[-]*[0-9]+[<>=][=]*[a-z]+") in								(* To fix: only '=' is never returned by python libraries, equals is represented with '=='. *)
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let stringTest = Str.matched_string stringGuard in
		let regExp = (Str.regexp "[-]*[0-9]+") in
		if ((testSearching stringTest regExp) == -1) then "ERROR" else
			let guardValue = Str.matched_string stringTest in
			let regExp = (Str.regexp "[<>=][=]*") in
			if ((testSearching stringTest regExp) == -1) then "ERROR" else
				let guardType = Str.matched_string stringTest in
				let regExp = (Str.regexp "[a-z]+") in
				if ((testSearching stringTest regExp) == -1) then "ERROR" else
					let guardName = Str.matched_string stringTest in
					let regExp = (Str.regexp "[-]*[0-9]+[<>=][=]*[a-z]+") in
					let newGuard = (guardName ^ (reverse_guard_type guardType) ^ guardValue) in
					let stringUpdated = Str.replace_first regExp newGuard stringGuard in
					reverse_guard stringUpdated;;


(** #3.4 REPLACE UNCOMFORTABLE SYMBOLS **)
let replacing_uncomfortable stringGuard = 
	let stringUpdated = Str.global_replace (Str.regexp "<=") "$" stringGuard in
	let stringUpdated = Str.global_replace (Str.regexp ">=") "%" stringUpdated in
	Str.global_replace (Str.regexp "==") "=" stringUpdated;;


(** #3.5 ADDING GUARD SEPARATOR **)
let rec adding_open_guard_separator stringGuard = 
	let regExp = (Str.regexp "[^-{]*[a-z]+") in
	if ((testSearching stringGuard regExp) == -1) then stringGuard else
		let regExp = (Str.regexp "[a-z]+") in
		if ((testSearching stringGuard regExp) == -1) then stringGuard else		
		let stringMatched = Str.matched_string stringGuard in
		let stringSubstituted = "{" ^ stringMatched in
		let stringUpdated = Str.replace_first regExp stringSubstituted stringGuard in
		let position = ((String.index stringUpdated '}') + 1) in
		let thisFragment = String.sub stringUpdated 0 position in
		let nextFragment = String.sub stringUpdated position ((String.length stringUpdated)-position) in
		thisFragment^(adding_open_guard_separator nextFragment);;

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

let adding_guard_separator stringGuard =
	adding_open_guard_separator (adding_close_guard_separator stringGuard);;


(** #3.6 PYTHON MAIN PARSER **)
let rec python_parse_guardValue =
	parser
(*	  [< '',' ; x = python_parse_guardType ?? "ERROR" >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then raise (Stream.Error "ERROR") 
			else "\" />\n<guard id=\"" ^ x *)
  | [< ''}' >] -> "}"
	| [< 'x; y = python_parse_guardValue ?? "Error calculating value" >] -> (String.make 1 x ^ y)
and python_parse_guardType =
	parser
	| [< ''$'; x = python_parse_guardValue ?? raise (Stream.Error "Missing value after <=") >] -> "<=" ^ x
	| [< ''%'; x = python_parse_guardValue ?? raise (Stream.Error "Missing value after >=") >] -> ">=" ^ x
	| [< ''='; x = python_parse_guardValue ?? raise (Stream.Error "Missing value after ==") >] -> "=" ^ x
	| [< ''<'; x = python_parse_guardValue ?? raise (Stream.Error "Missing value after <")  >] -> "<" ^ x
	| [< ''>'; x = python_parse_guardValue ?? raise (Stream.Error "Missing value after >") >] -> ">" ^ x
	| [< 'x; y = python_parse_guardType ?? raise (Stream.Error "The case should not occur1") >] -> String.make 1 x ^ y
and python_parse_guards =
	parser
	| [< x = python_parse_guardType >] -> x 
and python_parser =
	parser
	| [< ''{'; x = python_parse_guards ?? raise (Stream.Error "Missing guard"); >] -> "G{" ^ x
	| [< ''|'; x = python_parser ?? raise (Stream.Error "Missing first OR branch"); y = python_parser ?? raise (Stream.Error "Missing second OR branch") >] -> "<int>" ^ x ^ y ^ "</int>"
	| [< ''&'; x = python_parser ?? raise (Stream.Error "Missing first AND branch"); y = python_parser ?? raise (Stream.Error "Missing second AND branch") >] -> "<ext>" ^ x ^ y ^ "</ext>"
	| [< 'x; y = python_parser ?? raise (Stream.Error "The case should not occur2") >] -> String.make 1 x ^ y	(* The case should not occur *)
;;

python_parser (Stream.of_string ("|&{t-s<-2}{x<4}&"));;
python_parser (Stream.of_string ("||&{t-s<-2}{x<4}&{t>4}&{s-t$2}{x<4}&{s<6}&{s-t$2}&{t$4}{x<4}"));;


(* 0) Take Python output. *)
let a = "(c.t-c.s<-2 & c.x<4) | (4<c.t & c.s-c.t<=2 & c.x<4) | (c.s<6 & c.s-c.t<=2 & c.t<=4 & c.x<4)\n";;


(* 1) Remove the final '\n' *)
let b = String.sub a 0 ((String.length a)-1);;
"(c.t-c.s<-2 & c.x<4) | (4<c.t & c.s-c.t<=2 & c.x<4) | (c.s<6 & c.s-c.t<=2 & c.t<=4 & c.x<4)";;


(* 1) Remove context name. *)
let c = remove_context b;;
"(t-s<-2 & x<4) | (4<t & s-t<=2 & x<4) | (s<6 & s-t<=2 & t<=4 & x<4)";;


(* 2) Infix to Prefix. *)
let d = python_infix_to_prefix c;;
"||&t-s<-2x<4&4<t&s-t<=2x<4&s<6&s-t<=2&t<=4x<4";;


(* 3) Reverse guards. *)
let e = reverse_guard d;;
"||&t-s<-2x<4&t>4&s-t<=2x<4&s<6&s-t<=2&t<=4x<4";;


(* 4) Replacing symbols uncomfortable: '<=' became '$', '>=' became '%' and '==' became '='. *)
let f = replacing_uncomfortable e;;
"||&t-s<-2x<4&t>4&s-t$2x<4&s<6&s-t$2&t$4x<4";;


(* 5) Adding guard separator. *)
let g = adding_guard_separator f;;
"||&{t-s<-2}{x<4}&{t>4}&{s-t$2}{x<4}&{s<6}&{s-t$2}&{t$4}{x<4}";;


(* 6) Parsing result. *)





(** 						SECTION #4								**)
(** OCAML INTERFACE TO PYTHON LIBRARIES. 	**)

(** #4.1 PAST: CALLS PYTHON FUNCTION 'DOWN' **)
(* It takes a guard, then calls python libraries and executes the 'down' function. It returns a new guard. *)
let past guard = 
	let clocksNames = clocksNamesFromGuard guard in
	let guardName = "a" in 
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName guard)^python_print^guardName^python_down^python_command_end in 
	toGuard (syscall command);;


(** #4.2 INVRESET: CALLS PYTHON FUNCTION 'INVRESET' **)
(* It takes a guard and a clock, then calls python libraries and executes the 'invReset' function. It returns a new guard. *)
let invReset guard clock = 
	let clocksNames = clocksNamesFromGuard guard in
	let declaredClock = pythonStringOfClock clock in 
	if (exists declaredClock clocksNames) 
		then
			(
				let guardName = "a" in 
				let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName guard)^python_print^guardName^python_invReset_start^"c."^declaredClock^python_invReset_end^python_command_end in 
				toGuard (syscall command)
			)
		else failwith _ERR_200;;


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