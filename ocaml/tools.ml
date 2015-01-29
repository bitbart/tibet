(** 
 ********************************************************************************
 **																																						 **
 **				TOOLS (1): Offers functions used in different files.								 **
 **																																						 **
 ********************************************************************************
 **)

(*--------------------------------------------------
    OCAML TOPLEVEL IMPORTS (for Eclipse )

#load "str.cma";;
--------------------------------------------------*)

(* It looks for a regular expression in a string: if there is no match, function returns -1, *)
(* else it returns the position of the first result in the string. It avoids warnings when calling *)
(* Str.search_forward but the integer value returned is not intresting for the programmer: *)
(* now he can simply verifies "if((testSearching stringInput regExp) != -1)" to compile without warnings. *)
let testSearching stringInput regExp =
  try Str.search_forward regExp stringInput 0 with Not_found -> -1;;