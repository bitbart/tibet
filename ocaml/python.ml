(** 
 ********************************************************************************
 **																																						 **
 **				PYTHON (3): Offers functions to use python libraries      					 **
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

(** #1 PYTHON SOURCE STRINGS **)
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


(** #1 SYSTEM CALLS **)
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


(** #1 STRING TOOLS **)
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





(** 						SECTION #4								**)
(** OCAML INTERFACE TO PYTHON LIBRARIES. 	**)

(** #4 PAST: CALLS PYTHON FUNCTION 'DOWN' **)
(* It takes a guard, then calls python libraries and executes the 'down' function. It returns a new guard. *)
let past guard = 
	let clocksNames = clocksNamesFromGuard guard in
	let guardName = "a" in 
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName guard)^python_print^guardName^python_down^python_command_end in 
	toGuard (syscall command);;


(** #4 INVRESET: CALLS PYTHON FUNCTION 'INVRESET' **)
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


(** #4 SUBTRACT: CALLS PYTHON OPERATION '-' **)
(* It takes two guards, then call python operation '-'. It returns a new guard. *)
let subtract guard' guard'' =
	let clocksNames' = clocksNamesFromGuard guard' in
	let clocksNames'' = clocksNamesFromGuard guard'' in
	let guardName' = "a" in
	let guardName'' = "b" in
	let clocksNames = compress (List.sort comparatorStrings (clocksNames'@clocksNames'')) in
	let command = python_command_start^(pythonContextDeclaration clocksNames)^(pythonGuardDeclaration guardName' guard')^(pythonGuardDeclaration guardName'' guard'')^python_print^guardName'^python_subtract^guardName''^python_command_end in 
	toGuard (syscall command);;


(** #4 EQUIVALENCE: CALLS PYTHON OPERATION '==' **)
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