(** 
 ********************************************************************************
 **																																						 **
 **				FROMXML (4): offers functions for reading xml contracts              **
 **																																						 **
 ********************************************************************************
 **)

(* It requires xml-light module - download it and exec 'make install' *)

(* Inclusions to be used when compiling with Ocaml Interactive Environment *)
(* #use "toXML.ml";; *)

(* Inclusions to be used when compiling with makefile *)
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
		fromIntChoiceToList (getSequence children) @ getIntChoice ic'
	| Xml.Element ("rec", attrs, children)::ic' -> 
		fromIntChoiceToList (getSequence children) @ getIntChoice ic'
	| Xml.Element ("call", attrs, children)::ic' -> 
		fromIntChoiceToList (getSequence children) @ getIntChoice ic'
	| _ -> []
and getExtChoice ec = 
	match ec with
	| Xml.Element ("extaction", attrs, children)::ec' -> 
		let id = List.assoc "id" attrs in
		[(TSBAction id, getActionGuards children, getActionResets children, Success)] @ getExtChoice ec'
	| Xml.Element ("sequence", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| Xml.Element ("extchoice", attrs, children)::ec' ->
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| Xml.Element ("rec", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| Xml.Element ("call", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| _ -> []
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