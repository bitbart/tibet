(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *) 
open Python;;
open Tipi;;
open ExtTipi;;




(** 							SECTION #1						**)
(** THE FULL LIST OF ELEMENTS TO TEST.	**)
(* Guards. *)
let guardList = 
	[
		(And(SC(TSBClock "x", ExtLess, 4),DC (TSBClock "x", TSBClock "t", ExtLessEq, 7)));
		(Or(SC(TSBClock "x", ExtEq, 4),DC (TSBClock "x", TSBClock "t", ExtGreatEq, 7)));
		(Or(SC(TSBClock "t", ExtEq, 4), Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));
		(Or(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));
		(And(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));
		(Or(And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));
		(Or(SC(TSBClock "t", ExtEq, 4), And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));
		(Or(SC(TSBClock "t", ExtEq , 4), And(SC(TSBClock "x", ExtEq, 5), SC (TSBClock "s", ExtEq, 6))));
		(And(Or(SC(TSBClock "t", ExtEq , 4),SC(TSBClock "x", ExtEq, 5)),  SC (TSBClock "s", ExtEq, 6)))
	];;

(* Clocks. *)
let clockList = 
	[
		TSBClock "x";
	];;



(** 									SECTION #2									**)
(** TEST: PAST, INVRESET, SUBTRACT, EQUIVALENCE.	**)
(* Test a single element with a given function. *)
let testElement functionTest element =
	try let x = (functionTest element) in (ignore x; true) with _ -> false;;

(* Test a list of elements with a given function. *)
let rec testElementList functionTest elementList  = 
	match elementList with
	| [] -> true
	| h::t -> (testElement functionTest h) && (testElementList functionTest t);;

(* Test a couple of elements with a given function. *)
let testCouple functionTest element1 element2 =
	try let x = (functionTest element1 element2) in (ignore x; true) with _ -> false;;

(* Test a list of couples. *)
let rec testCoupleList functionTest coupleList  = 
	match coupleList with
	| [] -> true
	| (h1, h2)::t -> (testCouple functionTest h1 h2) && (testCoupleList functionTest t);;

(* Given a list of element, returns all the possible couples. *)
let makeCouples elementList =
	(	
	let rec makeCouples' elementList originalList =
		(
			match elementList with
				| [] -> []
				| h::t -> 
					(
					let rec makeCouples'' element list = 
						match list with
							| [] -> []
							| (h'::t') -> (element, h')::(makeCouples'' element t')
					in makeCouples'' h originalList
					)@(makeCouples' t originalList)
		) in makeCouples' elementList elementList
	);;

(* Testing Past. *)
let testPast =
	let x = assert ((testElementList past guardList) == true) in
	ignore x; "Done";;

(* Testing InvReset. *)
let testInvReset = "ToDO";;

(* Testing Subtract. *)
let testSubtract =
	let x = assert ((testCoupleList subtract (makeCouples guardList)) == true) in
	ignore x; "Done";;

(* Testing Equivalence. *)
let testEquivalence =
	let x = assert ((testCoupleList equivalence (makeCouples guardList)) == true) in
	ignore x; "Done";;

(* Main test function that launches all tests. *)
let testPython =
	let x = testPast in ignore x; 
	let x = testSubtract; in ignore x;
	let x = testEquivalence in ignore x; "Done";;