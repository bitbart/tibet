(*#load "dynlink.cma";;
#load "camlp4o.cma";;*)

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
	| [< 'x; y = parse_resets >] -> Char.escaped x ^ y
and parse_guardValue =
	parser
	  [< '',' ; x = parse_guardType ?? "Missing a valid guard after symbol ',' in at least one guard field" >] -> "\" />\n<guard id=\"" ^ x
	| [< ''}' >] -> "\" />\n</guards>"	
	| [< '';'; x = parse_resets ?? "Missing a valid clock id after symbol ';' in at least one reset field" >] -> "\" />\n</guards>\n<resets>\n<reset id=\"" ^ x
	| [< 'x; y = parse_guardValue ?? "Missing '}' or ';' after a guard" >] -> Char.escaped x ^ y
and parse_guardType =
	parser
	  [< ''<' ; x = parse_guardValue ?? "3" >] -> "\" op=\"less\" value=\"" ^ x
	| [< ''>' ; x = parse_guardValue ?? "4" >] -> "\" op=\"great\" value=\"" ^ x
	| [< 'x; y = parse_guardType ?? "5" >] -> Char.escaped x ^ y
and parse_guards' =
	parser
		[< '',' >] -> failwith "Missing a valid clock id after symbol ';' in at least one reset field"
	| [< ''}' >] -> "\n</guards>\n<resets />"
	| [< x = parse_resets >] -> "</guards>\n<resets>" ^ x
and parse_guards =
	parser
	  [< ''}' >] -> "\n</guards>\n<resets />"
	| [< '';' ; x = parse_guards' >] -> x
	| [< x = parse_guardType >] -> "\n<guard id=\"" ^ x 
and parse_contract' =
	parser
    [< ''?'; x = parse_contract' >] -> "\n<extaction id=\"" ^ x ^ "\n</extaction>"
	| [< ''!'; x = parse_contract' >] -> "\n<intaction id=\"" ^ x ^ "\n</intaction>"
	| [< ''{'; g = parse_guards >] -> "\">\n<guards>" ^ g
	| [< ''#'; x = parse_contract'; y = parse_contract' >] -> "\n<intchoice>" ^ x ^ y ^ "\n</intchoice>"
	| [< ''+'; x = parse_contract'; y = parse_contract' >] -> "\n<extchoice>" ^ x ^ y ^ "\n</extchoice>"
	| [< ''.'; x = parse_contract'; y = parse_contract' >] -> "\n<sequence>" ^ x ^ y ^ "\n</sequence>"
	| [< 'x; y = parse_contract' >] -> (Char.escaped x) ^ y
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
			else s ^ "\n" ^ s' ^ "\n" ^ remove_empties' l'
	| s :: [] -> s
	| [] -> ""
;;  

let rec remove_empties s = remove_empties' (Str.split (Str.regexp "[\n]+") s);;

let parse_contract c = remove_empties ("<contract>" ^ parse_contract' (Stream.of_string (infix_to_prefix c)) ^ "\n</contract>");;


(**** TEST_MODE ****)
let s = "(!pay{x<5,x<4} . ?send{} . !pay{y<4})";;
let c = parse_contract s;;



