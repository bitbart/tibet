(** This file will contain the code for the conversion from XML to abstract sintax **)
(* Simple version, it doesn't support recursion and other complex contract trees *)

(* It requires xml-light module - download it and exec 'make install'
#load "xml-light.cma";;
#use "toXML.ml";;*)

open Tipi;;
open Mapping;;
open ToXML;;

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
	| Xml.Element ("rec", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| Xml.Element ("call", attrs, children)::ec' -> 
		fromExtChoiceToList (getSequence children) @ getExtChoice ec'
	| _ -> []
and getExtChoice ec = 
	match ec with
	| Xml.Element ("extaction", attrs, children)::ec' -> 
		let id = List.assoc "id" attrs in
		[(TSBAction id, getActionGuards children, getActionResets children, Success)] @ getExtChoice ec'
	| Xml.Element ("sequence", attrs, children)::ec' -> 
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
		let id = List.assoc "id" attrs in
		Rec(id, getSequence children) 
	| Xml.Element ("call", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		Call id 
	| _ -> Success
;;

let getChildren c =
	match c with
	| Xml.Element ("sequence", attrs, children)::c' -> getSequence children
	| Xml.Element ("intchoice", attrs, children)::c' -> IntChoice (getIntChoice children)
	| Xml.Element ("extchoice", attrs, children)::c' ->	ExtChoice (getExtChoice children)
	| Xml.Element ("intaction", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		IntChoice[(TSBAction id, getActionGuards children, getActionResets children, Success)]
	| Xml.Element ("extaction", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		ExtChoice[(TSBAction id, getActionGuards children, getActionResets children, Success)]
	| Xml.Element ("rec", attrs, children)::c' ->
		let id = List.assoc "id" attrs in
		Rec(id, getSequence children)
	| _ -> failwith "ERR01: Invalid element found in XML!"
;;

let readXmlContract contr = 
	let myfile = (* Xml.parse_string (Xml.to_string_fmt *) (Xml.parse_string contr) (*)*) in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> getChildren c
  | _ -> failwith "ERR00: Not valid contract XML file!"
;;

let readXmlContract_fromFile f = 
	let myfile = (*Xml.parse_string (Xml.to_string_fmt*) (Xml.parse_file f) (*)*) in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> getChildren c
  | _ -> failwith "ERR00: Not a valid contract XML file!"
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
		