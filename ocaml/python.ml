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
let python_down = "a.down();";;


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


(** OCAML INTERFACE FOR PYTHON LIBRARIES **)
(* It takes a extGuard, then returns a list of its clocks names. *)
let clocksNamesFromExtGuard extGuard = 
	["x"];;

(* It takes a list of clocks names and uses it to create the initial python command, that is the istruction: 'c = Context c =([...])' *)
let pythonContextInstruction clocksNames =
	let rec pythonContextInstruction' clocksNames command =
		(
			match clocksNames with
			| h::t -> pythonContextInstruction' t (command^"\""^h^"\", ")
			| [] -> (String.sub command 0 ((String.length command)-2))^python_context_end
		) in
	pythonContextInstruction' clocksNames python_context_start;;


(** PARSER FROM EXTENDED GUARD TO PYTHON DELCARATION **)
(* It takes a extGuard and returns the string with the python instruction used to declare the guard: 'a = (c.x<10)'. *)
let pythonDeclarationGuard extGuard = "a=(c.x<10); ";;


(** PARSER FROM PYTHON DELCARATION TO EXTENDED GUARD **)
(* It takes the output received by python libraries and converts it in a extGuard. *)
let toExtGuard pythonOutput =
	pythonOutput;;


(** PAST: CALLS PYTHON FUNCTION 'DOWN' **)
(* It takes a extGuard, then calls python libraries and executes the 'down' function. It returns a new extGuard. *)
let past extGuard = 
	let clocksNames = clocksNamesFromExtGuard extGuard in
	let command = python_command_start^(pythonContextInstruction clocksNames)^(pythonDeclarationGuard extGuard)^python_print^python_down^python_command_end in 
	toExtGuard (syscall command);;


(** INVRESET: CALLS PYTHON FUNCTION 'INVRESET' **)
(* It takes a extGuard and a clock, then calls python libraries and executes the 'invReset' function. It returns a new extGuard. *)
let invReset extGuard clock = 
	let command = "" in 
		toExtGuard (syscall command);;

(* Test: past "g{x; y; z}";; *)