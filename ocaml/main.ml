(** 
 ********************************************************************************
 **																																						 **
 **				MAIN (6): contains a simple interface to run CTU converter           **
 **																																						 **
 ********************************************************************************
 **)

(* Inclusions to be used when compiling with makefile - DO NOT COMPILE THIS FILE IN OCAML TOPLEVEL! *)
open Tipi;;
open Mapping;;
open ToXML;;
open FromXML;;
open Cparser;;



(** READ_INPUT **)
(*  Reads from standard input and convert it to a string list *)
let rec read_input' s c chan =
	let s' = input_line chan in
	if ((String.compare s' "") == 0) then read_input' s c chan
	else if ((String.compare s' "</contract>") == 0 && c == 0) then [s^s'] @ (read_input' "" 1 chan)
	else if (String.compare s' "</contract>") == 0 then [s^s']
	else read_input' (s ^ s') c chan
;;

let read_input chan = read_input' "" 0 chan;;



(** MAIN **)
(*  An interface for using CTU converter from Unix command-line *)
let main = 
	let argn = (Array.length Sys.argv) in
	match argn with
		| 1 -> 
			let rc = read_input stdin in
			contractsToAutomata (List.hd rc) (List.hd (List.rev rc))
		| 2 ->
			(
			match (Sys.argv.(1)) with
			| "-s" -> print_string (parse_multiple_contracts (input_line stdin))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| 4 -> 
			(
			match (Sys.argv.(1)) with
			| "-ff" -> contractsToAutomata_fromFile (Sys.argv.(1)) (Sys.argv.(2))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| _ -> print_string ("Wrong input!\n\nPlease use:\n\n\t'$ ./ctu -ff file1.txt file2.txt' to convert two xml contracts in Uppaal's xml\n\t'$ ./ctu -s < file.txt' to convert a string contract in a XML contract")
;;

		