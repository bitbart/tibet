(** This file will contain the code for the conversion from XML to abstract sintax **)
(* Simple version, it doesn't support recursion and other complex contract trees *)

(* It requires xml-light module - download it and exec 'make install' *)
#load "xml-light.cma";;

let getOperator op =
	match op with
	| "less" -> Less
	| "great" -> Great
	| _ -> failwith "Invalid operator in automaton guard"
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
	| _ -> []
;;

let rec getExtChoice ic = 
	match ic with
	| Xml.Element ("extaction", attrs, children)::ic' -> 
		let id = List.assoc "id" attrs in
		[(TSBAction id, getActionGuards children, getActionResets children, Success)] @ getExtChoice ic'
	| _ -> []
;;

let rec getSequence s =
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
	| _ -> failwith "Invalid element found"
;;

let readXmlContract filename = 
	let myfile = Xml.parse_file filename in
  match myfile with
  | Xml.Element ("contract", attrs, c) -> getChildren c
  | _ -> failwith "Not valid contract XML file"
;;

let contractToAutomaton = readXmlContract "ex07_c1.xml";;
let contractToAutomaton = readXmlContract "ex07_c2.xml";;