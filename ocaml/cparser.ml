#load "dynlink.cma";;
#load "camlp4o.cma";;

let rec sts p =
	try
		let c = Stream.next p in
		Char.escaped c ^ sts p
	with
	| Stream.Failure -> ""	
;;

let rec parse_resets =
		parser
		[< '',' ; x = parse_resets ?? "1" >] -> "\" />\n<reset id=\"" ^ x
	| [< ''}' >] -> "\" />\n</resets>"
	| [< 'x; y = parse_resets >] -> Char.escaped x ^ y
;;

let rec parse_guardValue =
	parser
	  [< '',' ; x = parse_guardType ?? "1" >] -> "\" />\n<guard id=\"" ^ x
	| [< ''}' >] -> "\" />\n</guards>"	
	| [< '';'; x = parse_resets ?? "2" >] -> "\" />\n<resets>\n<reset id=\"" ^ x
	| [< 'x; y = parse_guardValue ?? "2" >] -> Char.escaped x ^ y
and parse_guardType =
	parser
	  [< ''<' ; x = parse_guardValue ?? "3" >] -> "\" op=\"less\" value=\"" ^ x
	| [< ''>' ; x = parse_guardValue ?? "4" >] -> "\" op=\"great\" value=\"" ^ x
	| [< 'x; y = parse_guardType ?? "5" >] -> Char.escaped x ^ y
;;

let rec parse_guards =
	parser
	  [< ''}' >] -> "\n</guards>"
	| [< '';' ; x = parse_resets >] -> "</guards>\n<resets>" ^ x
	| [< x = parse_guardType >] -> "\n<guard id=\"" ^ x 
;;

let rec parse_contract' =
	parser
    [< ''?'; y = parse_contract' >] -> "<extaction id=\"" ^ y ^ "\n</extaction>"
	| [< ''!'; y = parse_contract' >] -> "<intaction id=\"" ^ y ^ "\n</intaction>"
	| [< ''{'; y = parse_guards >] -> "\">\n<guards>" ^ y
	| [< 'x; y = parse_contract' >] -> (Char.escaped x) ^ y
;;

let parse_contract c = "<contract>\n" ^ parse_contract' (Stream.of_string c) ^ "\n</contract>";;

(** REMOVE_SPACES **)
let rec remove_spaces' =
	parser
	  [< '' '; x = remove_spaces' >] -> x
	| [< ''*' >] -> ""
	| [< 'x; y = remove_spaces' >] -> Char.escaped x ^ y
;;

let remove_spaces s = remove_spaces' (Stream.of_string (s^"*"));;

(*let s = "!pay{x<5,x<4;x,y,z} . ? send{x<8} + ! wait{x<5,x<4;x,y} . ! pay{y<4}";;
let c = remove_spaces s;;

let c1 = parse_contract "!pay{x<5,x<4;x,y,z}.?send{x<8}+!wait{x<5,x<4;x,y}.!pay{y<4}";;
let c2 = parse_contract "?pay{x<6}";;

contractsToAutomata c1 c2;;


print_string c1;;
print_string c2;;
_____________________________________________________________________________ *)		 

let s = "!pay{x<5,x<4;x,y,z} . ? send{x<8} + ! wait{x<5,x<4;x,y} . ! pay{y<4}";;
let c = remove_spaces s;;

let getOp_prio op =
	match op with
	| '+' -> 5
	| '.' -> 6
	| ')' -> 2
	| '(' -> 2
	| _ -> 0
;;

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

update_stack ("", ['.';'.';'+';'+'; ')']) (getOp_prio ')');;

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
	  | '.' ->
			let (output, new_stack) = update_stack ("", stack) (getOp_prio '.') in
			output ^ infix_to_prefix' s' ('.'::new_stack)
		| '(' -> let (output, new_stack) = update_stack ("", stack) (getOp_prio '(') in
			output ^ infix_to_prefix' s' new_stack
	  | _ -> (Char.escaped c) ^ infix_to_prefix' s' stack
;;

let infix_to_prefix s = reverse(infix_to_prefix' (reverse (remove_spaces (s))) []);;





