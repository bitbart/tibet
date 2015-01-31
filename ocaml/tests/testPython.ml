(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *) 
open Python;;
open Tipi;;
open ExtTipi;;




(** 						SECTION #1						**)
(** THE FULL LIST OF GUARDS TO TEST.	**)
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





(** 									SECTION #2									**)
(** TEST: PAST, INVRESET, SUBTRACT, EQUIVALENCE.	**)
(* Test a single element with a single function. *)
let test' functionTest guard =
	try let x = (functionTest guard) in (ignore x; true) with _ -> false;;

(* Test multiple elements with a single function. *)
let rec test functionTest guardList  = 
	match guardList with
	| [] -> true
	| h::t -> (test' functionTest h) && (test functionTest t);;

(* Testing past. *)
let testPast =
	let x = assert ((test past guardList) == true) in
	ignore x; "Done";