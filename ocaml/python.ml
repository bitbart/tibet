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

*)

(* Inclusion to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINE *)
open Unix;;

(** STRINGS PYTHON SOURCE **)
let python_command_start = "python -c '";;
let python_command_end = "'";;
let python_context_start = "from python_dbm import Context; c = Context([";;
let python_context_end = "], \"c\"); ";;
let python_print = "print ";;
let python_down = "a.down(); ";;
let python_invReset_start = "a.invReset(";;
let python_invReset_end = "); ";;


(** SYSTEM CALLS **)
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


(** COPARSER FROM EXTENDED GUARD TO PYTHON DELCARATION **)
(* It returns the name of a clock. *)
let pythonStringOfClock clock = 
	match clock with
	| TSBClock c -> c;;

(* If 'list' contains 'element' then returns true else false. *)
let rec exists element list = match list with
  | [] -> false
  | h::t -> if ((String.compare element  h) == 0) then true 
    else exists element t;;

(* It compares two strings. This is used to eliminate duplicates from the list of clocks declaration. *)
let comparatorStrings s1 s2 =
	if(s1 > s2) then 1
	else if (s1 == s2) then 0
	else -1;;

(* It eliminates all duplicates from the list of clocks declaration. *)
let rec compress clocksList =
	match clocksList with
	| a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;

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
			) in compress (List.sort comparatorStrings (clocksNamesFromGuard' inputGuard []));;

(* It takes a list of clocks names and uses it to create the initial python command, that is the istruction: 'c = Context c =([...])' *)
let pythonContextInstruction clocksNames =
	let rec pythonContextInstruction' clocksNames command =
		(
			match clocksNames with
			| h::t -> pythonContextInstruction' t (command^"\""^h^"\", ")
			| [] -> (String.sub command 0 ((String.length command)-2))^python_context_end
		) in
	pythonContextInstruction' clocksNames python_context_start;;

(* Returns the string representation of a relation. *)
let pythonStringOfTsbRelation relation = 
	match relation with
	| ExtLess -> "<"
	| ExtGreat -> ">"
	| ExtLessEq -> "<="
	| ExtGreatEq -> ">="
	| ExtEq -> "=";;

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

(* It takes a guard and returns the string with the python instruction used to declare the guard: 'a = (c.x<10)'. *)
let pythonDeclarationInstruction guard = 
	"a=" ^ (pythonGuardFromGuard guard) ^ ";";; 


(** PARSER FROM PYTHON DELCARATION TO GUARD **)
(* It takes the output received by python libraries and converts it in a guard. *)
let toGuard pythonOutput =
	pythonOutput;;


(** OCAML INTERFACE TO PYTHON LIBRARIES **)
(** PAST: CALLS PYTHON FUNCTION 'DOWN' **)
(* It takes a guard, then calls python libraries and executes the 'down' function. It returns a new guard. *)
let past guard = 
	let clocksNames = clocksNamesFromGuard guard in
	let command = python_command_start^(pythonContextInstruction clocksNames)^(pythonDeclarationInstruction guard)^python_print^python_down^python_command_end in 
	toGuard (syscall command);;


(** INVRESET: CALLS PYTHON FUNCTION 'INVRESET' **)
(* It takes a guard and a clock, then calls python libraries and executes the 'invReset' function. It returns a new guard. *)
let invReset guard clock = 
	let clocksNames = clocksNamesFromGuard guard in
	let declaredClock = pythonStringOfClock clock in 
	if (exists declaredClock clocksNames) 
		then
			(
				let command = python_command_start^(pythonContextInstruction clocksNames)^(pythonDeclarationInstruction guard)^python_print^python_invReset_start^"c."^declaredClock^python_invReset_end^python_command_end in 
				toGuard (syscall command)
			)
		else failwith _ERR_200;;