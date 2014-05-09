(** 
 ********************************************************************************
 **																																						 **
 **				CPARSER (5): contains a converter from string contracts to XML ones  **
 **																																						 **
 ********************************************************************************
 **)

(* When compiling with makefile, use ocamlopt -c -pp camlp4o cparser.ml to activate Camlp4 Preprocessing *)

(* Inclusions to be used when compiling with Ocaml Interactive Environment *)
(* 
#load "dynlink.cma";;
#load "camlp4o.cma";;
 *)

let printc c =
	let s = Char.escaped c in 
	if (Str.string_match (Str.regexp "[a-z]") s 0) then s 
	else failwith "Invalid format for at least one action id or rec id  or clock id. Only a-z allowed.";;

let printv c =
	let s = Char.escaped c in 
	if (Str.string_match (Str.regexp "[0-9]") s 0) then s 
	else failwith "Invalid format for at least one clock value. Only integer values allowed.";;

let rec sts p =
	try
		let c = Stream.next p in
		Char.escaped c ^ sts p
	with
	| Stream.Failure -> ""	
;;

let rec parse_resets =
		parser
		[< '',' ; x = parse_resets ?? "Missing a valid clock id after symbol ',' in at least one reset field" >] -> "\" />\n<reset id=\"" ^ x
	| [< ''}' >] -> "\" />\n</resets>"
	| [< 'x; y = parse_resets ?? "Missing '}' or ',' in at least one reset field" >] -> (printc x) ^ y
and parse_guardValue =
	parser
	  [< '',' ; x = parse_guardType ?? "Missing a valid guard after symbol ',' in at least one guard field" >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then failwith "A clock id cannot have an empty identifier!" 
			else "\" />\n<guard id=\"" ^ x
	| [< ''}' >] -> "\" />\n</guards>"	
	| [< '';'; x = parse_resets ?? "Missing a valid clock id after symbol ';' in at least one reset field" >] -> "\" />\n</guards>\n<resets>\n<reset id=\"" ^ x
	| [< 'x; y = parse_guardValue ?? "Missing '}' or ';' after a guard" >] -> (printv x) ^ y
and parse_guardType =
	parser
	  [< ''<' ; x = parse_guardValue  >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then failwith "Missing a valid clock value after symbol '<' in at least one guard field" 
			else "\" op=\"less\" value=\"" ^ x
	| [< ''>' ; x = parse_guardValue >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then failwith "Missing a valid clock value after symbol '>' in at least one guard field" 
			else "\" op=\"great\" value=\"" ^ x
	| [< 'x; y = parse_guardType ?? "Missing a valid guard in at least one guard field" >] -> (printc x) ^ y
and parse_guards' =
	parser
		[< '',' >] -> failwith "Missing a valid clock id after symbol ';' in at least one reset field"
	| [< ''}' >] -> "\n</guards>\n<resets />"
	| [< x = parse_resets >] -> "</guards>\n<resets><reset id=\"" ^ x
and parse_guards =
	parser
	  [< ''}' >] -> "\n</guards>\n<resets />"
	| [< '';' ; x = parse_guards' ?? "Missing a valid reset in at least one reset field" >] -> x
	| [< x = parse_guardType >] -> "\n<guard id=\"" ^ x 
and parse_call =
	parser
    [< '']'; x = parse_contract' >] -> x 
	| [< 'x; y = parse_call >] -> y
and parse_contract' =
	parser
    [< ''?'; x = parse_contract' ?? "Missing a valid action name after symbol '?'" >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then failwith "An action cannot have an empty identifier!" 
			else "\n<extaction id=\"" ^ x ^ "\n</extaction>"
	| [< ''!'; x = parse_contract' ?? "Missing a valid action name after symbol '!'" >] -> 
			if ((String.compare "\"" (String.sub x 0 1)) == 0) then failwith "An action cannot have an empty identifier!" 
			else "\n<intaction id=\"" ^ x ^ "\n</intaction>"
	| [< ''R'; ''E'; ''C'; ''['; x = parse_contract' ?? "Missing variable name in REC" >] -> "\n<rec name=\"" ^ x ^ "\n</rec>"
	| [< '']'; x = parse_contract' ?? "Missing a valid action after REC[] operator" >] -> "\">" ^ x
	| [< ''['; x = parse_call >] -> x
	| [< ''{'; g = parse_guards >] -> 			
			if ((String.compare "\n<guard id=\"\"" (String.sub g 0 13)) == 0) then failwith "A clock id cannot have an empty identifier!" 
			else "\">\n<guards>" ^ g
	| [< ''#'; x = parse_contract'; y = parse_contract' >] -> "\n<intchoice>" ^ x ^ y ^ "\n</intchoice>" (* TODO: check for recursion as child - not allowed*)
	| [< ''+'; x = parse_contract'; y = parse_contract' >] -> "\n<extchoice>" ^ x ^ y ^ "\n</extchoice>"
	| [< ''.'; x = parse_contract'; y = parse_contract' >] -> "\n<sequence>" ^ x ^ y ^ "\n</sequence>"
	| [< 'x; y = parse_contract' >] -> (printc x) ^ y
;;

(** REMOVE_SPACES **)
let rec remove_spaces' =
	parser
	  [< '' '; x = remove_spaces' >] -> x
	| [< ''*' >] -> ""
	| [< 'x; y = remove_spaces' >] -> Char.escaped x ^ y
;;

let remove_spaces s = remove_spaces' (Stream.of_string (s^"*"));;

(** GET_OPERATOR_PRIORITIES **)
let getOp_prio op =
	match op with
	| '+' -> 5
	| '#' -> 5
	| '.' -> 6
	| ')' -> 2
	| '(' -> 2
	| _ -> 0
;;

(** REVERSE string **)
let rec reverse s =
	match s with
	| "" -> ""
	| _ ->
		let new_len =  (String.length s) - 1 in
		(Char.escaped (String.get s new_len)) ^ (reverse (String.sub s 0 new_len))
;;

let rec update_stack res pr =
	let (s, l) = res in
		match l with
		| c::l' ->
			if (getOp_prio c >= pr) then 
				(if (pr==2 && (getOp_prio c) == 2) then (s, l') else update_stack (s^(Char.escaped c), l') pr)
			else res
		| [] -> res
;; 

let rec infix_to_prefix' s stack =
	match s with
	| "" -> 
		let (output, l) = update_stack ("", stack) 0 in output
	| _ ->
		let c = String.get s 0 in
		let s' = String.sub s 1 ((String.length s) - 1) in
		match c with
		| ')' -> infix_to_prefix' s' (')'::stack)
		| '+' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '+') in
			output ^ infix_to_prefix' s' ('+'::new_stack)
		| '#' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '#') in
			output ^ infix_to_prefix' s' ('#'::new_stack)
	  | '.' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '.') in
			output ^ infix_to_prefix' s' ('.'::new_stack)
		| '(' -> let (output, new_stack) = update_stack ("", stack) (getOp_prio '(') in
			output ^ infix_to_prefix' s' new_stack
	  | _ -> (Char.escaped c) ^ infix_to_prefix' s' stack
;;

let infix_to_prefix s = reverse(infix_to_prefix' (reverse (remove_spaces (s))) []);;

let rec remove_empties' l =
	match l with
	| s :: s' :: l' -> 
			if (String.compare s "<guards>" == 0 && String.compare s' "</guards>" == 0) then remove_empties' l' 
			else if (String.compare s "<resets />" == 0 || String.compare s "<resets/>" == 0) then remove_empties' (s'::l')
			else if (String.compare s' "<guards>" == 0) then s ^ "\n" ^ remove_empties' (s'::l') 
			else s ^ "\n" ^ s' ^ "\n" ^ remove_empties' l'
	| s :: [] -> s
	| [] -> ""
;;  

let rec remove_empties s = remove_empties' (Str.split (Str.regexp "[\n]+") s);;

let preprocess_rec s = 
	let s' = Str.global_replace (Str.regexp "\\(REC\\[[a-z]+\\]\\)\\(\\.\\)") "\\1(" s in
	Str.global_replace (Str.regexp "\\(\\.\\)\\(\\[[a-z]+\\]\\)") ")\\2" s';;

let postprocess_rec s = Str.global_replace (Str.regexp "\\(<rec name=\\\"\\)\\([a-z]+\\)\\(\\\">\\)\\([^\\&]+\\)\\(</rec>\\)") "\\1\\2\\3\\4<call name=\"\\2\" />\\5" s;;

let parse_contract c = remove_empties ("<contract>" ^ parse_contract' (Stream.of_string (infix_to_prefix (preprocess_rec c))) ^ "\n</contract>");;

let rec parse_multiple_contracts' l =
	match l with
	| c::l' -> (postprocess_rec (parse_contract c)) ^ "\n" ^ parse_multiple_contracts' l' 
	| [] -> ""
;;

let parse_multiple_contracts c = parse_multiple_contracts' (Str.split (Str.regexp "[|]+") c);;


