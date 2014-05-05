open Tipi;;
open Mapping;;
open ToXML;;
open FromXML;;
open Cparser;;

(** READ_INPUT **)
(*  Legge dallo standard input un contenuto xml e lo trasforma in lista di stringhe/righe *)
let rec read_input' s c chan =
	let s' = input_line chan in
	if ((String.compare s' "") == 0) then read_input' s c chan
	else if ((String.compare s' "</contract>") == 0 && c == 0) then [s^s'] @ (read_input' "" 1 chan)
	else if (String.compare s' "</contract>") == 0 then [s^s']
	else read_input' (s ^ s') c chan
;;

let read_input chan = read_input' "" 0 chan;;

(** MAIN **)
(*  It encodes two contract xml files, passed as parameters or from stdin *)
(*  IMPORTANT: if you use stdin, the two contracts must be entered in sequence *)
let main = 
	let argn = (Array.length Sys.argv) in
	match argn with
		| 1 -> 
			let rc = read_input stdin in
			contractsToAutomata (List.hd rc) (List.hd (List.rev rc))
		| 2 ->
			(
			match (Sys.argv.(1)) with
			| "-t" -> print_string (parse_contract (input_line stdin))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| 4 -> 
			(
			match (Sys.argv.(1)) with
			| "-ff" -> contractsToAutomata_fromFile (Sys.argv.(1)) (Sys.argv.(2))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| _ -> print_string ("Wrong input!\nYou can use:\n\n\t'$ ctu' and insert xml contracts from stdin:\n\t-u")
;;

		