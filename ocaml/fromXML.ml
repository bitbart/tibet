(** 
 ********************************************************************************
 **																																						 **
 **				FROMXML (4): offers functions for reading xml contracts              **
 **																																						 **
 ********************************************************************************
 **)

(* Requires xml-light module - download it and exec 'make install' *)

(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *)
open Tipi;;
open Mapping;;
open ToXML;;
open Errors;;








let rec fromIntChoiceToList (IntChoice l) = l;;

let rec fromExtChoiceToList (ExtChoice l) = l;;

let getOperator op =
	match op with
	| "less" -> Less
	| "great" -> Great
	| _ -> failwith "ERR02: Invalid operator in automaton guard!"
;;

let rec getSingleGuards g =
	match g with
	| Xml.Element ("guard", attrs, children)::g' -> 
		let id = List.assoc "id" attrs in
		let op = List.assoc "op" attrs in
		let v = int_of_string (List.assoc "value" attrs) in
		[(TSBClock id, getOperator op, v)] @ getSingleGuards g'
	| _ -> []
;;

let rec getSingleResets r =
	match r with
	| Xml.Element ("reset", attrs, children)::r' -> 
		let id = List.assoc "id" attrs in
		[TSBClock id] @ getSingleResets r'
	| _ -> []
;;

let getActionResets ac = 
	match ac with
	| Xml.Element ("guards", attrs, children)::ac' -> 
		(match ac' with
		| Xml.Element ("resets", attrs, children)::ac' -> TSBReset (getSingleResets children)
		| _ -> TSBReset[])
	| Xml.Element ("resets", attrs, children)::ac' -> TSBReset (getSingleResets children)
	| _ -> TSBReset[]
;;

let getActionGuards ac = 
	match ac with
	| Xml.Element ("guards", attrs, children)::ac' -> TSBGuard (getSingleGuards children)
	| _ -> TSBGuard[]
;;

let rec getIntChoice ic = 
	match ic with
	| Xml.Element ("intaction", attrs, children)::ic' -> 
		let id = List.assoc "id" attrs in
		[(TSBAction id, getActionGuards children, getActionResets children, Success)] @ getIntChoice ic'
	| Xml.Element ("sequence", attrs, children)::ic' ->
		fromIntChoiceToList (getSequence children) @ getIntChoice ic'
	| Xml.Element ("intchoice", attrs, children)::ic' ->
		getIntChoice children @ getIntChoice ic'
	| [] -> []
	| _ -> failwith "ERR401: Invalid element found in intchoice!"
and getExtChoice ec = 
	match ec with
	| Xml.Element ("extaction", attrs, children)::ec' -> 
		let id = List.assoc "id" attrs in
		[(TSBAction id, getActionGuards children, getActionResets children, Success)] @ getExtChoice ec'
	| Xml.Element ("sequence", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| Xml.Element ("extchoice", attrs, children)::ec' ->
		getExtChoice children @ getExtChoice ec'
	| [] -> []
	| _ -> failwith "ERR402: Invalid element found in extchoice!"
and getSequence s =
	match s with
	| Xml.Element ("intaction", attrs, children)::s' -> 
		let id = List.assoc "id" attrs in
		IntChoice[(TSBAction id, getActionGuards children, getActionResets children, getSequence s')]
	| Xml.Element ("extaction", attrs, children)::s' -> 
		let id = List.assoc "id" attrs in
		ExtChoice[(TSBAction id, getActionGuards children, getActionResets children, getSequence s')]
	| Xml.Element ("intchoice", attrs, children)::s' -> 
		IntChoice (getIntChoice children)
	| Xml.Element ("extchoice", attrs, children)::s' -> 
		ExtChoice (getExtChoice children)
	| Xml.Element ("rec", attrs, children)::c' ->
		let id = List.assoc "name" attrs in
		Rec(id, getSequence children) 
	| Xml.Element ("call", attrs, children)::c' ->
		let id = List.assoc "name" attrs in
		Call id 
	| Xml.Element ("sequence", attrs, children)::c' ->
		getSequence children 
	| _ -> Success
;;

let getChildren c =
	match c with
	| Xml.Element ("sequence", attrs, children)::c' -> getSequence c
	| Xml.Element ("intchoice", attrs, children)::c' -> IntChoice(getIntChoice c)
	| Xml.Element ("extchoice", attrs, children)::c' ->	ExtChoice(getExtChoice c)
	| Xml.Element ("intaction", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		IntChoice[(TSBAction id, getActionGuards children, getActionResets children, Success)]
	| Xml.Element ("extaction", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		ExtChoice[(TSBAction id, getActionGuards children, getActionResets children, Success)]
	| Xml.Element ("rec", attrs, children)::c' ->
		let id = List.assoc "name" attrs in
		Rec(id, getSequence children)
	| _ -> failwith "ERR01: Invalid element found in XML!"
;;

let preprocess_contract s = 
	Str.global_replace (Str.regexp "\\(\\<\\)") "\n\\1" s
;;

let readXmlContract contr = 
	let myfile = Xml.parse_string contr in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> getChildren c
  | _ -> failwith "ERR00: Not valid contract XML file!"
;;

let readXmlContract_fromFile f = 
	let myfile = Xml.parse_file f in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> getChildren c
  | _ -> failwith "ERR00: Not a valid contract XML file!"
;;

(* Checks if a variable x (called from CALL) is declared in the recursive variables list (updated from REC). *)
let rec removeVariableFromList x l = 
	match l with
	| [] -> false
	| h::t -> 
			if((compare h x) == 0) 
				then true
				else removeVariableFromList x t
;;

let readXmlContract_special contr = 
	let myfile = Xml.parse_string contr in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> IntChoice(getIntChoice c)
  | _ -> failwith "ERR00: Not valid contract XML file!"
;;

(* Checks variable binding (between REC and CALL) in a contract. *)
let checkRecursion contractInput =
	let rec checkRecursion2 contractInput listVariables = 
		match contractInput with
		| Success -> true
		| IntChoice((a, g, r, t)::l') -> checkRecursion2 t listVariables && checkRecursion2 (IntChoice(l')) listVariables
	 	| ExtChoice((a, g, r, t)::l') -> checkRecursion2 t listVariables && checkRecursion2 (ExtChoice(l')) listVariables
		| Rec(x, y) -> 
			let listUpdated = x::listVariables in checkRecursion2 y listUpdated
		| Call(x) -> removeVariableFromList x listVariables 
		| _-> true in
	checkRecursion2 contractInput []
;;

let contractsToAutomata_fromFile f f' =
	let p = readXmlContract_fromFile f in
	let q = readXmlContract_fromFile f' in
	let lta = tsb_mapping p q in
	writeTAstd lta
;;

let contractsToAutomata c c' =
	let p = readXmlContract c in
	let q = readXmlContract c' in
	let lta = tsb_mapping p q in
	writeTAstd lta
;;

let rec print_intchoice ic = 
	match ic with
	| Xml.Element ("intaction", attrs, children)::ic' -> 
		Xml.to_string (Xml.Element ("intaction", attrs, children)) ^ print_intchoice ic'
	| Xml.Element ("sequence", attrs, children)::ic' -> 
		"<sequence>" ^ print_sequence children ^ "</sequence>" ^ print_intchoice ic'
	| Xml.Element ("intchoice", attrs, children)::ic' ->
		print_intchoice children ^ print_intchoice ic'
	| [] -> ""
	| _ -> failwith "ERR12: Invalid element found in XML (in remove_nested intchoice)!"
and print_extchoice ec = 
	match ec with
	| Xml.Element ("extaction", attrs, children)::ec' -> 
		Xml.to_string (Xml.Element ("extaction", attrs, children)) ^ print_extchoice ec'
	| Xml.Element ("sequence", attrs, children)::ec' -> 
		"<sequence>" ^ print_sequence children ^ "</sequence>" ^ print_extchoice ec'
	| Xml.Element ("extchoice", attrs, children)::ec' ->
		print_extchoice children ^ print_extchoice ec'
	| [] -> ""
	| _ -> failwith "ERR11: Invalid element found in XML (in remove_nested extchoice)!"
and print_sequence s =
	match s with
	| Xml.Element ("intaction", attrs, children)::s' -> 
		Xml.to_string (Xml.Element ("intaction", attrs, children)) ^ print_sequence s'
	| Xml.Element ("extaction", attrs, children)::s' -> 
		Xml.to_string (Xml.Element ("extaction", attrs, children)) ^ print_sequence s'
	| Xml.Element ("intchoice", attrs, children)::s' -> 
		"<intchoice>" ^ print_intchoice children ^ "</intchoice>" ^ print_sequence s'
	| Xml.Element ("extchoice", attrs, children)::s' -> 
		"<extchoice>" ^ print_extchoice children ^ "</extchoice>" ^ print_sequence s'
	| Xml.Element ("rec", attrs, children)::s' ->
		let n = List.assoc "name" attrs in
		"<rec name=\"" ^ n ^ "\">" ^ print_rec children ^ "</rec>" ^ print_sequence s'
	| Xml.Element ("call", attrs, children)::s' ->
		Xml.to_string (Xml.Element ("call", attrs, children)) ^ print_sequence s'
	| Xml.Element ("sequence", attrs, children)::s' ->
		print_sequence children ^ print_sequence s'
	| [] -> ""
	| _ -> failwith "ERR11: Invalid element found in XML (in remove_nested sequence)!"
and print_rec r =
		match r with
	| Xml.Element ("intaction", attrs, children)::s' -> 
		Xml.to_string (Xml.Element ("intaction", attrs, children)) ^ print_rec s'
	| Xml.Element ("extaction", attrs, children)::s' -> 
		Xml.to_string (Xml.Element ("extaction", attrs, children)) ^ print_rec s'
	| Xml.Element ("intchoice", attrs, children)::s' -> 
		"<intchoice>" ^ print_intchoice children ^ "</intchoice>" ^ print_rec s'
	| Xml.Element ("extchoice", attrs, children)::s' -> 
		"<extchoice>" ^ print_extchoice children ^ "</extchoice>" ^ print_rec s'
	| Xml.Element ("rec", attrs, children)::s' ->
		let n = List.assoc "name" attrs in
		"<rec name=\"" ^ n ^ "\">" ^ print_rec children ^ "</rec>" ^ print_rec s'
	| Xml.Element ("call", attrs, children)::s' ->
		Xml.to_string (Xml.Element ("call", attrs, children)) ^ print_rec s'
	| Xml.Element ("sequence", attrs, children)::s' ->
		print_sequence children ^ print_rec s'
	| [] -> ""
	| _ -> failwith "ERR11: Invalid element found in XML (in remove_nested rec)!"
;;

let removeNestedTag' c =
	match c with
	| Xml.Element ("sequence", attrs, children)::c' -> "<sequence>\n" ^ print_sequence children ^ "</sequence>"
	| Xml.Element ("intchoice", attrs, children)::c' -> "<intchoice>" ^ print_intchoice children ^ "</intchoice>"
	| Xml.Element ("extchoice", attrs, children)::c' ->	"<extchoice>" ^ print_extchoice children ^ "</extchoice>"
	| Xml.Element ("intaction", attrs, children)::c' -> Xml.to_string (Xml.Element ("intaction", attrs, children))
	| Xml.Element ("extaction", attrs, children)::c' -> Xml.to_string (Xml.Element ("extaction", attrs, children))
	| Xml.Element ("rec", attrs, children)::c' ->
		let name = List.assoc "name" attrs in "<rec name=\"" ^ name ^ "\">" ^ print_rec children ^ "</rec>"
	| _ -> failwith "ERR10: Invalid element found in XML (in remove_nested)!"
;;

let removeNestedTag xml = 
	let input = Xml.parse_string xml in
  match input with
  | Xml.Element ("contract", attrs, c) -> "<contract>" ^ removeNestedTag' c ^ "</contract>"
  | _ -> failwith "ERR00: Not valid contract XML file (in remove_nested)!"
;;
