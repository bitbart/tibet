(** 
 ********************************************************************************
 **																																						 **
 **				MAIN (11): Contains a simple interface to run CTU converter.         **
 **																																						 **
 ********************************************************************************
 **)

(* Inclusions to be used when compiling with makefile - PLEASE IGNORE THE FOLLOWING LINE
   open Tipi;;open ExtTipi;;open Mapping;;open Kindsystem;;open ToXML;;open FromXML;;open Cparser;;open Monitor;; *)

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
			| "-da" -> let rc = read_one_contract stdin in
          if (admitsCompliant (toExtTsb (readXmlContract rc))) then print_string("yes") else print_string("no")
      | "-dk" -> let rc = read_one_contract stdin in print_string(extGuardToString (kindof (toExtTsb (readXmlContract rc))))
      | "-dd" -> let rc = read_one_contract stdin in print_string(extTsbToString (dualof (toExtTsb (readXmlContract rc))))
	    | "-ba" -> let contract = readXmlContract (read_one_contract stdin) in
			             print_string (ta_to_string (buildAutomatonMain (toExtTsb contract) "p"))
	    | "-gl" -> let contract = readXmlContract (read_one_contract stdin) in
				          (
	                let set = getLabels (buildAutomatonMain (toExtTsb contract) "p") in
	                let rec printLabels l = (match l with
	                | [] -> ""
	                | (Label h)::t -> h^", "^(printLabels t)) in
	                print_string (printLabels set))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
    | 3 -> 
      (
      match (Sys.argv.(1)) with
      | "-start" -> 
						let rc = read_input stdin in
            start_mon (List.hd rc) (List.hd (List.rev rc)) (Sys.argv.(2))
			| "-ie" ->
						if (isEnded (Sys.argv.(2))) then print_string("yes") else print_string("no")
      | _ -> 
					print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
      )
		| 4 -> 
			(
			match (Sys.argv.(1)) with
			| "-ic" -> print_string (isCulpab (int_of_string (Sys.argv.(2))) (Sys.argv.(3)))
      | "-id" -> print_string (isOnDuty (int_of_string (Sys.argv.(2))) (Sys.argv.(3)))
			| "-pa" -> print_string (get_actions (Sys.argv.(3)) (int_of_string (Sys.argv.(2))))
			| "-ff" -> contractsToAutomata_fromFile (Sys.argv.(2)) (Sys.argv.(3))
			| _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| 5 -> 
      (
      match (Sys.argv.(1)) with
			| "-isa" -> print_string (isAllowed (Sys.argv.(2)) (int_of_string (Sys.argv.(3))) (Sys.argv.(4)))
      | "-delay" -> delay_net (float_of_string (Sys.argv.(2))) (Sys.argv.(3)) (Sys.argv.(4))
      | _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
      )	
    | 8 -> 
      (
      match (Sys.argv.(1)) with
      | "-step" -> fire_act (int_of_string (Sys.argv.(2))) (Sys.argv.(3)) (float_of_string (Sys.argv.(4))) (Sys.argv.(5)) (Sys.argv.(6)) (int_of_string (Sys.argv.(7)))
      | _ -> print_string ("Wrong input!\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
      )
		| _ -> print_string ("Wrong input!\n\nPlease use:\n\n\t'$ ./ctu -ff file1.txt file2.txt' to convert two xml contracts in Uppaal's xml\n\t'$ ./ctu -s < file.txt' to convert a string contract in a XML contract")
;;


(**

1) Per ogni TST  devi memorizzare: 
     (i)  la lista dei canali usati
     (ii) la sua rappresentazione a stringa
  Es: dato p un TST: 
    let aut = buildAutomatonMain p "p";;  --> crea l'automa
    getLabels aut;;                       --> restituisce la lista dei canali  
    - : label set = [Label "a"; Label "b"]
    ta_to_string (aut);;                  --> restituisce la codifica xml

*)