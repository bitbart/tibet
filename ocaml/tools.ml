(** 
 ********************************************************************************
 **																																						 **
 **				TOOLS (1): Offers functions used in different files.								 **
 **																																						 **
 ********************************************************************************
 **)

open Errors;;

(* It looks for a regular expression in a string: if there is no match, function returns -1, *)
(* else it returns the position of the first result in the string. It avoids warnings when calling *)
(* Str.search_forward but the integer value returned is not intresting for the programmer: *)
(* now he can simply verifies "if((testSearching stringInput regExp) != -1)" to compile without warnings. *)
let testSearching string regexp =
  try Str.search_forward regexp string 0 with Not_found -> -1;;

(* It reverses a string. *)
let rec python_reverse s =
	match s with
	| "" -> ""
	| _ ->
		let new_len =  (String.length s) - 1 in
		(String.make 1 (String.get s new_len)) ^ (python_reverse (String.sub s 0 new_len));;