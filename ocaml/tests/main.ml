(* Inclusions to be used when compiling with makefile - DO NOT COMPILE THIS FILE IN OCAML TOPLEVEL! *)
open TestPython;;

(** MAIN **)
(*  An interface for test CTU from Unix command-line *)
let main = 
	let argn = (Array.length Sys.argv) in
	match argn with
		| 2 ->
			(
				match (Sys.argv.(1)) with
				| "-p" -> print_string ("Testing Python:\n\t");
									print_string (testPast ^ "\n")
				| _ -> 		print_string ("Wrong input.\nUnrecognized or misused option: " ^ (Sys.argv.(1)) ^ "\n")
			)
		| _ -> print_string ("Wrong input.")
;;