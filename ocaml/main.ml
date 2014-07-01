(** 
 ********************************************************************************
 **																																						 **
 **				MAIN (7): contains a simple interface to run CTU converter           **
 **																																						 **
 ********************************************************************************
 **)

(* Inclusions to be used when compiling with makefile - DO NOT COMPILE THIS FILE IN OCAML TOPLEVEL! *)
open Tipi;;
open Mapping;;
open ToXML;;
open FromXML;;
open Cparser;;
open Monitor;;




let find_termination s = 
	if (Str.string_match (Str.regexp "[a-z0-9\\\"\\/\\<\\>= ]*\\<\\/contract\\>[a-z0-9\\\"\\/\\<\\>= ]+") s 0)
	then failwith "Wrong input! Please start a new line after each </contract> tag" 
	else Str.string_match (Str.regexp "[a-z0-9\\\"\\/\\<\\>= ]*\\<\\/contract\\>") s 0;; 

(** READ_INPUT **)
(*  Reads from standard input and convert it to a string list *)
let rec read_input' s c chan =
	let s' = input_line chan in
	if ((String.compare s' "") == 0) then read_input' s c chan
	else if (find_termination s' && c == 0) then [s^s'] @ (read_input' "" 1 chan)
	else if (find_termination s') then [s^s']
	else read_input' (s ^ s') c chan
;;

let read_input chan = read_input' "" 0 chan;;

let rec read_one_contract' s chan =
	let s' = input_line chan in
	if ((String.compare s' "") == 0) then read_one_contract' s chan
	else if (find_termination s') then s^s'
	else read_one_contract' (s ^ s') chan
;;

let read_one_contract chan = read_one_contract' "" chan;;


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
			| "-v" -> 
				let rc = read_one_contract stdin in
				if (checkRecursion (readXmlContract rc))
				then
					print_string ("Contract is valid")
				else
					print_string ("Contract is not valid")
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| 4 -> 
			(
			match (Sys.argv.(1)) with
			| "-ic" -> print_string (isCulpable (int_of_string (Sys.argv.(2))) (Sys.argv.(3)))
      | "-id" -> print_string (isOnDuty (int_of_string (Sys.argv.(2))) (Sys.argv.(3)))
			| "-ff" -> contractsToAutomata_fromFile (Sys.argv.(2)) (Sys.argv.(3))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| 5 -> 
      (
      match (Sys.argv.(1)) with
      | "-start" -> start_mon (Sys.argv.(2)) (Sys.argv.(3)) (Sys.argv.(4))
      | _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
      )
    | 7 -> 
      (
      match (Sys.argv.(1)) with
      | "-step" -> fire_act (int_of_string (Sys.argv.(2))) (Sys.argv.(3)) (float_of_string (Sys.argv.(4))) (Sys.argv.(5)) (Sys.argv.(6))
      | _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
      )
		| _ -> print_string ("Wrong input!\n\nPlease use:\n\n\t'$ ./ctu -ff file1.txt file2.txt' to convert two xml contracts in Uppaal's xml\n\t'$ ./ctu -s < file.txt' to convert a string contract in a XML contract")
;;

		