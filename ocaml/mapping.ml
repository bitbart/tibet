(** 
 ********************************************************************************
 **                                                                            **
 **MAPPING (4): Offers functions which map contracts into automata.            **
 **																																						 **
 ********************************************************************************
 **)

(*-------------------------------------------------- 
   OCAML TOPLEVEL IMPORTS (for Eclipse )

		#load "xml-light.cma";;
		#load "dynlink.cma";;
		#load "camlp4o.cma";;
*)
(*
----------------------------------------------------
	 OCAML TOPLEVEL IMPORTS (for Emacs)
 
	 #load "str.cma";;
   #use "tipi.ml";;
   #use "extTipi.ml";;
----------------------------------------------------
*) 


(* Inclusion to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINE *)
open Tipi;;
open ExtTipi;;
 

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Mapping extTsb into UPPAAL automata                                           *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)
(*                                                                                                  *)
(* 1) let extTsb_mapping p q  =  [ buildAutomatonMain p "p" ; buildAutomatonMain q "q"] ;;          *)
(* transforms a extTsb into an automaton                                                            *)
(* *)
(* 2) let tsb_mapping p q  =  [ buildAutomatonMain ( toExtTsb  p) "p" ; buildAutomatonMain ( toExtTsb  q) "q"] ;;*)
(* convert a tsb into  a  extTsb and transforms it into an automaton*)
(* *)
(* This mapping uses tsb in defining equations normal form (denf) *)
(* Tsb are checked in order to:*)
(* 1) normalize all the guards   -- this is done during the tranformation into denf*)
(* 2) there are no free recursion variables*)
(* 3) there are no rec in not guarded-sequeces*)
(**)
(*The mapping maintains the following limitations:*)
(* A) guards shall not have disjunctions not negations*)
(* B) only one clock may have upperbound in all the guards of an internal choice  *)

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Normalizing guards                                                            *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)
(* We normalize guard and make sure that for every clock, there is one window frame/Interval to fire the action*)
(* We need to transform t<4, t<7, t<10 into t<4;   and t<4, t >7 into t <0 *)
(* so that every clock a singlo interval of the form:*)
(* t>d1 and t<d2  (with <= or >= as well) OR *)
(* t< d OR *)
(* t > d*)
(* t = d*)

let neverSatisfiedConstraint c = (c, ExtLess, 0);;
let alwaysSatisfiedConstraint  c  = ( c, ExtGreatEq, 0);;

(*Partition the guard in sets of simple constraints belonging to the same clock*)
let rec partition clocks l = match clocks with 
  [] -> []
| hd::tl ->(hd,  List.filter (fun (TSBClock c, a, b)-> if hd = c then true else false ) l):: (partition  tl l);;


(*Checking if the two constraint generate an interval  NOT empty : es:  t< 4, t >6 *)
let notEmptyIntervalBis a b  = match (a,b) with 
  (c1, ExtGreatEq, d1),(c2, ExtLessEq, d2) ->   d1 <= d2  (* es: t >= 3, t < 3*) 
| (c1, ExtGreat, d1),(c2, r2, d2)          ->   d1 < d2   (* es: t > 3, t < 5*) 
| (c1, r1, d1),(c2, ExtLess, d2)           ->   d1 < d2
| (c1, ExtLessEq, d1),(c2, ExtGreatEq, d2) ->   d2 <= d1  
| (c1, ExtLess, d1),(c2, r2, d2)          ->    d2 < d1   
|_-> failwith "isEmptyInterval: Unexpected element"
;;

(*Checking if the interval is NOT empty : meaning t<0 or t< 4, t >6 *)
(*the list at maximum contains 3 clauses*)
let notEmptyInterval l = match l with 
    []-> true
| (c, r, d)::[] -> true
| (c1, r1, d1)::(c2, r2, d2)::[]-> notEmptyIntervalBis (c1, r1, d1) (c2, r2, d2) 
|_-> failwith "isEmptyInterval: Unexpected element"
;;

let isEmptyInterval l =  (notEmptyInterval l == false)  ;; 

(*Get the clock list clocks from a set of clock constraints l*)
let rec getClockListFromGuard  g  = match g with 
  True -> []
| False -> []
| SC (TSBClock t, rel, d) ->  [t]
| And (g1,g2) ->  addSetSet (getClockListFromGuard g1 ) (getClockListFromGuard g2) 
| _ -> failwith "getClockListFromList: to be implemented" 
;;


(*Transform the guard to a list of constrants--- CONJUNCTION ONLY*)
let rec guardToList g  = match g with 
  True -> []
| False -> []
| SC (TSBClock t, rel, d) ->  [(TSBClock t, rel, d)]
| And (g1,g2) ->  addSetSet (guardToList g1 ) (guardToList g2) 
| _ -> failwith "guardToList: to be implemented" 
;;

(*Transform the list to a guard--- CONJUNCTION ONLY*)
let rec listToGuard l  = match l with 
   [] -> True
| (c, r, d)::[] ->  SC ( c, r, d)
| (c, r, d)::tl ->  And ( SC ( c, r, d)  , listToGuard tl   ) 
;;

(*getFalse searches for *false* constraints and keep them*)
(* es: false returns true while t<0 return false*) 
let rec isFalseInGuard g  = match g with 
| True -> false
| False -> true
| And (g1,g2) ->  isFalseInGuard g1 || isFalseInGuard g2 
| SC (TSBClock t, rel, d) -> false
| _ -> failwith "isFalseInGuard"
;;


(*Returs the intersection between two  bounds on the same clock:*)
(* es: intersect t<1 t<6 returns t<6*)
(* es: intersect t>1 t>6 returns t>6*)
let intersect  b1 b2 = match (b1,b2) with 
| (clock1, ExtGreat, d1), (clock2, ExtGreat,d2)->    (if d1 < d2 then b2 else b1)   
| (clock1, ExtGreat, d1), (clock2, ExtGreatEq,d2)->  (if d1 < d2 then b2 else ( if d1 = d2 then b1 else b1))          
| (clock1, ExtGreatEq, d1), (clock2, ExtGreat,d2)->  (if d1 < d2 then b2 else ( if d1 = d2 then b2 else b1))
| (clock1, ExtGreatEq, d1), (clock2, ExtGreatEq,d2)->  (if d1 < d2 then b2 else b1)
| (clock1, ExtLess, d1), (clock2,ExtLess,d2)->     (if d1 < d2 then b1 else b2)
| (clock1, ExtLess, d1), (clock2,ExtLessEq,d2)->   (if d1 < d2 then b1 else  ( if d1 = d2 then b2 else b2))
| (clock1, ExtLessEq, d1), (clock2,ExtLess,d2)->   (if d1 < d2 then b1 else  ( if d1 = d2 then b1 else b2))
| (clock1, ExtLessEq, d1), (clock2,ExtLessEq,d2)-> (if d1 < d2 then b1 else b2) 
| (clock1, ExtEq, d1), (clock2,ExtLessEq,d2)->     (if d1 <= d2 then b1 else  (neverSatisfiedConstraint clock1)  ) 
| (clock1, ExtEq, d1), (clock2,ExtLess,d2)->       (if d1 < d2 then b1 else  (neverSatisfiedConstraint clock1)   )
| (clock1, ExtEq, d1), (clock2,ExtGreatEq,d2)->    (if d1 >= d2 then b1 else (neverSatisfiedConstraint  clock1)  ) 
| (clock1, ExtEq, d1), (clock2,ExtGreat,d2)->      (if d1 > d2 then b1 else  (neverSatisfiedConstraint clock1)   )
| (clock1, ExtLessEq, d1), (clock2,ExtEq,d2)->     (if d1 >= d2 then b2 else  (neverSatisfiedConstraint clock1)  ) 
| (clock1, ExtLess, d1), (clock2,ExtEq,d2)->       (if d1 > d2 then b2 else  (neverSatisfiedConstraint clock1)   )
| (clock1, ExtGreatEq, d1), (clock2,ExtEq,d2)->    (if d1 <= d2 then b2 else  (neverSatisfiedConstraint clock1)  ) 
| (clock1, ExtGreat, d1), (clock2,ExtEq,d2)->      (if d1 < d2 then b2 else  (neverSatisfiedConstraint clock1)   )
|  _ ->  failwith "Something went wrong with intersect"
;;

(*takes a valid interval and intersects it with a point*)
(* es t <20 and t > 4 and t = 5*)
let rec intersectPointWithInterval p interval  = match interval with 
  []-> [p]
| (c, r, d)::tl-> let lb = intersectPointWithInterval p  tl in
                  if List.length lb = 1 then  [ intersect  (c, r, d) (List.nth lb 0) ]
                  else failwith " Error in intersectPointWithInterval" 
;;

(*takes a list with lower bounds only or upper bound only *)
(*returns a list containig the biggest lowerbound. ie: t > 2 and t > 4 gives t > 4*)
(*returns a list containig the lowest upperbound. ie: t < 10 and t < 4 gives t < 4*)
let rec getBoundary l = match l with 
  []-> []
| (c, r, d)::tl-> let lb = getBoundary tl 
                  in if lb = [] then [(c, r, d)]
                     else [ intersect  (c, r, d) (List.nth lb 0)]
;;


(*returns a list containig some constraints indicating an interval. *)
(*For instance (t =0) , otherwise (t<20 and t>2) otherwise (t >4) *)
(* The interval is valid (meaning no such as t < 10 , t > 20*)
let  getInterval clock part = let equalities = eliminateDuplicates (List.filter(fun (c, r, d) -> r = ExtEq ) part)  in
                               let upperBounds = List.filter(fun (c, r, d) -> r = ExtLessEq ||r = ExtLess  ) part in
                               let lowerBounds = List.filter(fun (c, r, d) -> r = ExtGreatEq ||r = ExtGreat  ) part in
                               let bounds      = (getBoundary lowerBounds)@(getBoundary upperBounds) in
                               let interval    = if isEmptyInterval bounds  then [neverSatisfiedConstraint (TSBClock clock)]  else  bounds
                               in (*if there are no eualities we are done*)
                                   if List.length equalities = 0 then interval
                                   else (*otherwise checking that the equality fits the intersection es t =5 and t < 30*)
                                       if List.length equalities = 1 then intersectPointWithInterval (List.nth equalities 0) interval 
                                       else [neverSatisfiedConstraint  (TSBClock clock)] (*too many equalities t = 3 and t = 9 -> [] *) 
;;  
  
(*Normalizing extGuard: making intervals on every sublist for every clock*) 
let normalize_guard (TSBExtGuard g) =    if isFalseInGuard g then TSBExtGuard False
                           else (
                             let cl = getClockListFromGuard g  in
                             (*per ogni clock ottengo la lista dei constr*)
                             let part = partition cl (guardToList g) in
                             (*genero intervalli validi per ogni clock*)
                             let intervalList =  List.map (fun x ->   getInterval (fst x) (snd x)) part 
                             in  TSBExtGuard (listToGuard( List.flatten intervalList))
                           )
;;


(*PROVA DEBUG*)
let g =  TSBExtGuard (And( And( SC(TSBClock "x", ExtEq, 5), SC(TSBClock "x",ExtGreat, 7)),
                           And( SC(TSBClock "t", ExtLessEq, 8), SC(TSBClock "t",ExtLessEq, 9)) ) );;
let g2 =  TSBExtGuard (True );;
let g3 =  TSBExtGuard (And( And( SC(TSBClock "x", ExtEq, 5), SC(TSBClock "x",ExtEq, 7)),
                           And( SC(TSBClock "t", ExtLessEq, 8), SC(TSBClock "t",ExtGreatEq, 9)) ) );;
normalize_guard g2;;

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Omega invariant for internal choice:                                          *)
(*            "you  wait as long as  there is still  something you can do"                          *)
(*                                                                                                  *)
(****************************************************************************************************)

let isUpperBoundRelation r = (r = ExtLess || r = ExtLessEq || r = ExtEq);;
let getClockName (TSBClock c) = c;;

let rec constraintListToString gl = match gl with 
  [] -> ""
| (c,r,d)::tl1::tl2-> getClockName(c)^extRelationToString(r)^(string_of_int d)^" && "^(constraintListToString (tl1::tl2))
| (c,r,d)::tl1  ->   getClockName(c)^extRelationToString(r)^(string_of_int d);;


(*If we have two upper bounds, we choose the one that gives more time, like in union of intervals*)
(* es: []   any -> t > 0  *)
(* es: [t<0]   any -> any  *)
(*es:  [t<4]   [t<7] ->   t < 7  *) 
(*In case of an equalit, it is changed into: es: t = 5 , t<4 -> t <=5*)
let getTheMoreDelayed c b1 b2 =  match (b1,b2) with 
  [] , _ |  _, [] ->   [alwaysSatisfiedConstraint (TSBClock  c)]
| [(clock1, ExtGreatEq, d1)], d  | d, [(clock1, ExtGreatEq, d1)] ->   [(clock1, ExtGreatEq, d1)]
| [(clock1, ExtLess, d1)], [(clock2,ExtLess,d2)]    ->  (if d1 < d2 then b2 else b1)
| [(clock1, ExtLess, d1)], [(clock2,ExtLessEq,d2)]  ->  (if d1 < d2 then b2 else  ( if d1 = d2 then b1 else b1))
| [(clock1, ExtLessEq, d1)], [(clock2,ExtLess,d2)]  ->  (if d1 < d2 then b2 else  ( if d1 = d2 then b2 else b1))
| [(clock1, ExtLessEq, d1)], [(clock2,ExtLessEq,d2)]->  (if d1 < d2 then b2 else b1) 
| [(clock1, ExtEq, d1)], [(clock2,ExtLess,d2)]  
| [(clock1, ExtEq, d1)], [(clock2,ExtLessEq,d2)]->   (if d1 < d2 then b2 else [(clock1, ExtLessEq, d1 )])  
| [(clock1, ExtLess, d1)], [(clock2,ExtEq,d2)]  
| [(clock1, ExtLessEq, d1)], [(clock2,ExtEq,d2)]->   (if d1 > d2 then b1 else [(clock1, ExtLessEq, d2 )])  
|  _ ->  failwith "Something went wrong with getTheMoreDelayed"
;;

(*takes a normalized guard so if there is an upper bound, it is only one *)
let rec extractTheUpperBoundFromGuard g = match g with 
  SC (TSBClock t, rel, d) ->  if (isUpperBoundRelation rel) then [(TSBClock t, rel, d)] else []
| And (g1,g2) ->  (extractTheUpperBoundFromGuard g1) @   (extractTheUpperBoundFromGuard g2)
| True | False  -> []
| _  ->  failwith "extractUpperBound:  to be implemented" 
;;

(*torna una lista di un unico constrant*)
let rec getBoundOfAllTheActions  c l  = match l with 
  [] ->  [neverSatisfiedConstraint (TSBClock c)]  
| (a, TSBExtGuard g, r, e)::tl -> let uppCurr =  (if (isFalseInGuard g) then [neverSatisfiedConstraint (TSBClock c)] 
                                                  else  extractTheUpperBoundFromGuard  g) in (*cercare l'upper bound. se esiste e' unico*)
                                  let uppNext = getBoundOfAllTheActions c tl 
                                  in  getTheMoreDelayed c  uppCurr uppNext  (*tengo quello che mi fa avanzare per piu' tempo*)
;;

(*let g =   (And( SC(TSBClock "t", ExtGreatEq, 10), SC(TSBClock "x", ExtGreatEq, 5)) );;*)
(*getBoundOfAllTheActions  "t" [(1,g,1,1)   ; (2,g,2,2)];;*)
(* let uppCurr =  (if (isFalseInGuard g) then [isFalseInGuard "x"] else  extractUpperBound  g);;*)

let rec getClocksWithUpperBound l = match l with 
   [] -> []
|  (TSBClock c, r, d)::tl -> addSetSet (if (isUpperBoundRelation r) then [c] else [])  (getClocksWithUpperBound tl) ;;

let rec getClocksWithUpperBoundInChoice l = match l with 
   [] -> []
|  (a, TSBExtGuard gl, r, e)::tl -> let c = (getClocksWithUpperBound (guardToList gl ) ) 
                                 in   addSetSet c   (getClocksWithUpperBoundInChoice tl) ;;

(*Guards are normalized so if there is an upper bpund, there is only one upper bound for each guard.*)
(* Moreover we check that there is only one cloc with upper bounds*)
(*in all the guard of all the branches*)
let getOmega l =  let cl = getClocksWithUpperBoundInChoice  l 
                  in if List.length cl > 1 then failwith " getOmega: more than a clock with upper bound in a choice"
                     else if cl = [] then "" 
                          else  constraintListToString (getBoundOfAllTheActions (List.nth cl 0 ) l)  ;; 


(*PROVA DEBUG*)
let g1 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,5), (SC(TSBClock "x",ExtGreatEq,2))));;
let g2 = TSBExtGuard (SC(TSBClock "x",ExtGreatEq,6));;
let g3 = TSBExtGuard (SC(TSBClock "x",ExtLess,3));;
let g4 =  TSBExtGuard (And(  SC(TSBClock "x", ExtEq, 5),
                           And( SC(TSBClock "t", ExtGreatEq, 8), SC(TSBClock "t",ExtGreatEq, 9)) ) );;
let g5 =  TSBExtGuard (And( And( SC(TSBClock "x", ExtEq, 5), SC(TSBClock "x",ExtEq, 7)),
                           And( SC(TSBClock "t", ExtGreatEq, 8), SC(TSBClock "t",ExtGreatEq, 9)) ) );;
normalize_guard g4;;
let l = [(1,normalize_guard g1,1,1) ; (2,normalize_guard g2,2,2); 
         (3,normalize_guard g3,3,3);(4,normalize_guard g4,4,4);(5,normalize_guard g5,5,5); ];;
getOmega  l;;

(****************************************************************************************************)
(*                                                                                                  *)
(*                   Transforming tsb into defining equation normal form                            *)
(*                                                                                                  *)
(****************************************************************************************************)
(* we allow no recursion variable starting with XX.*)
(*Checking that recursion variables in tsb are not of the form "XX"*)
let idePrefix = "XX";;
let prefixLen = 2;;
let startCounter = 0;;
let rec checkIdesInIde x = 
          try
            if (String.length x < prefixLen) then true 
            else  if ((String.sub x 0 prefixLen) = idePrefix) then false 
                  else true
          with
           _ -> failwith "Errore qui : checkIdesInIde. " 
;;
let newRecursionVariable count = idePrefix^string_of_int count;;

let rec checkIdesInChoice l = match l with 
[] -> true
| (a,b,c,d)::tl -> checkIdesInTsb d && checkIdesInChoice tl

and  checkIdesInTsb t = match t with 
  ExtSuccess -> true
| ExtIntChoice l  | ExtExtChoice l  -> checkIdesInChoice l
| ExtRec  (x,p) -> checkIdesInTsb p && checkIdesInIde x
| ExtCall x -> checkIdesInIde x
| ExtNil  -> true
;;
 
(*substituting x  to y into the equation (z,e) for each branch in the choice*)
let rec  substituteRecursionVariableRec x y l = match l with 
 [] -> []
| (a, b, c, z):: tl ->   (a, b, c, (if z = y then x else z)):: (substituteRecursionVariableRec x y tl)
;;

let rec  substituteRecursionVariable (z , e)  x y  = match e with 
  DENil -> (if z = y then x else z), e 
| DESuccess  -> (if z = y then x else z), e 
| DEIntChoice l   ->     (if z = y then x else z), DEIntChoice (substituteRecursionVariableRec x y l)
| DEExtChoice l   ->     (if z = y then x else z), DEExtChoice (substituteRecursionVariableRec x y l)
;;

let applySubstitutionToList x y l  = (List.map (fun curr  -> substituteRecursionVariable curr x  y ) l );;

(* t is the tsb and cnt the counter for recursion variable names as XX0 XX1 XX2 XX3....*)
(*each frangment returns a couple (X,  [X = eq; ...], count) where the second item is a list of defining equations*)
(* and count is the index of the next free variable*)
let rec extTsbToDENFRec t count = match t with 
  ExtSuccess -> (newRecursionVariable count, [newRecursionVariable count, DESuccess], count+1)
| ExtNil  ->  (newRecursionVariable count, [newRecursionVariable count, DENil], count+1)
| ExtIntChoice l  ->  let x0 = newRecursionVariable count in
                      let (cl, eql , newCount ) = choiceToDENF l (count +1)
                      in ( x0 , (x0, DEIntChoice cl) :: eql, newCount) 
| ExtExtChoice l  ->  let x0 = newRecursionVariable count in
                      let (cl, eql , newCount ) = choiceToDENF l (count +1)
                      in ( x0 , (x0, DEExtChoice cl) :: eql, newCount) 
| ExtRec  (x,p) -> let (y, l, newCount) = extTsbToDENFRec p (count+1) in 
                   let x0 = newRecursionVariable count in
                         (x0 , applySubstitutionToList x0 x  ( applySubstitutionToList x0  y  l), newCount) 
| ExtCall  x -> ( x, [], count)

and

(*!!!!!WE NORMALIZE GUARDS !!!!!!!!!!!!!!!!!!!!!!!!!!*)
choiceToDENF l count = match l with 
  []  -> failwith "Error in choiceToDENF"
| (a,b,c,d)::[] ->   let (y2, l2 , newCount2) = extTsbToDENFRec d count
                 in  ([(a, normalize_guard b, c, y2)],  l2 , newCount2)    (*we return a fragment for the choice, the set of equations, and the next free id*)
| (a,b,c,d)::tl ->  let (y2, l2 , newCount2) = extTsbToDENFRec d count
                    in let ( lChoice ,  l3 , newCount3)  = choiceToDENF tl newCount2
                    in     ( (a,normalize_guard b,c,y2) :: lChoice,  l2 @ l3, newCount3 +1)
;;   

(*Converting tsb into a DENF*)
(*AND normalizing guards*)
let extTsbToDENF t = if (not(checkIdesInTsb t)) then failwith "Recurision variables  starting with XX are not allowed."
                      else let (a,b,c)  = extTsbToDENFRec t startCounter
                           in (a,b) 
                      ;;  

(*Debug *)
let g1 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,1), (SC(TSBClock "x",ExtGreatEq,2))));;
let t  = ExtIntChoice [(TSBAction "c", g1 , TSBReset [], ExtCall "X"); 
                       (TSBAction "d", TSBExtGuard True, TSBReset [], ExtSuccess)];;
let t1  = ExtRec ("X",  ExtIntChoice [(TSBAction "a", TSBExtGuard True, TSBReset [], t); 
                                      (TSBAction "b", TSBExtGuard True, TSBReset [], t)] );;
extTsbToDENF t1;;

normalize_guard g1;;








(****************************************************************************************************)
(*                                                                                                  *)
(*                   Patterns                                                                       *)
(*                                                                                                  *)
(****************************************************************************************************)
(****************************************************************************************************)
(*                                                                                                  *)
(*                    Idle Automaton                                                                *)
(*                                                                                                  *)
(****************************************************************************************************)
(*The idle automaton does nothing*)
let idleAutomaton (Loc l) = TimedAutoma ("",[Loc l], Loc l,[],[],[], [], [], [], [], [],  []);;

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Success Automaton                                                             *)
(*                                                                                                  *)
(****************************************************************************************************)
(*The success automaton  synchronize on self loop, to allow for the property not deadlock to be true*)
let successSync = "success";;
let successAutomaton (Loc l)  = let edges =  [ Edge ( Loc l, Label  (successSync^"?"), "","", Loc l);
                                      Edge ( Loc l, Label  (successSync^"!"), "","", Loc l)]
                       in TimedAutoma ("",[Loc l], Loc l,[Label successSync],edges,[], [], [], [], [], [],  []);;
(****************************************************************************************************)
(*                                                                                                  *)
(*                    Prefix  Automaton                                                             *)
(*                                                                                                  *)
(****************************************************************************************************)
(*The prefix automaton  prefixes a location to an automaton*)
let prefixAutomaton (Loc l) (Label lab) r (TimedAutoma (name, locations, init, labels, edges, invariants, 
                                  clocks, globalClocks,  committed, variables, globalVariables,  procedures)) =
              try 
                    let edg =   Edge ( Loc l, Label lab, "", r, init) in 
                    let chans =  if (String.length lab>0) then addElSet (Label (String.sub lab 0  (String.length lab -1 )))  labels else  labels
                    in TimedAutoma (name, Loc l :: locations, Loc l , chans, edg :: edges, invariants, 
                                    clocks, globalClocks,   l :: committed, variables, globalVariables, procedures)
              with 
                 _-> failwith ("Errore qui: prefixAutomaton. label:"^lab^".") 
;;



(****************************************************************************************************)
(*                                                                                                  *)
(*                    Internal Choice  Automa                                                                *)
(*                                                                                                  *)
(****************************************************************************************************)
(*The internalChoiceAutomaton  prefixes a location to a set of  automata*)
(* it takes a location, an invariant , a list of couples: guard and automaton*)
(* lAut is a list of couples : a guard and an automata*)

(*lAut e' una tupla che contiene anche l'automa. f serve ad estrarre l'automa dalla tupla*)
let unionOfLocations f lAut  = List.fold_right (fun a b -> getLocations (f a) @ b) lAut [];;
let unionOfLabels  f lAut  = List.fold_right (fun a b -> getLabels  (f a) @ b) lAut [];;
let unionOfEdges f lAut  = List.fold_right (fun a b -> getEdges  (f a) @ b) lAut [];;
let unionOfInvariants  f lAut  = List.fold_right (fun a b -> getInvariants  (f a) @ b) lAut [];;
let unionOfClocks f lAut  = List.fold_right (fun a b -> getClocks  (f a) @ b) lAut [];;
let unionOfGlobalClocks f lAut  = List.fold_right (fun a b -> getGlobalClocks  (f a) @ b) lAut [];;
let unionOfCommitted f lAut  = List.fold_right (fun a b -> getCommitted  (f a) @ b) lAut [];;
let unionOfCommitted f lAut  = List.fold_right (fun a b -> getCommitted  (f a) @ b) lAut [];;
let unionOfVariables f lAut  = List.fold_right (fun a b -> getVariables (f a) @ b) lAut [];;
let unionOfGlobalVariables f lAut  = List.fold_right (fun a b -> getGlobalVariables  (f a) @ b) lAut [];;
let unionOfProcedures f lAut  = List.fold_right (fun a b -> getProcedures  (f a) @ b) lAut [];;

let rec ic_generateAllTheBranches (Loc l)  lAut = match lAut with 
[] -> []
| ( TSBExtGuard g,  (TimedAutoma (name, locations, init, labels, edges, invariants, 
       clocks, globalClocks,  committed, variables, globalVariables,  procedures))):: tl 
   ->   Edge ( Loc l, Label "", extGuardToString g,"", init) :: ic_generateAllTheBranches (Loc l) tl ;;

(*lAut is a tuple (  g, aut)list *)
let internalChoiceAutomaton (Loc l)  g lAut  = 
                    let edgs =   ic_generateAllTheBranches ( Loc l ) lAut in
                    let inv =  (l, g) in
                    let f =  (fun (a,b) -> b) 
                    in TimedAutoma ("", Loc l :: unionOfLocations f lAut, Loc l , unionOfLabels f lAut, edgs@ unionOfEdges f lAut, inv :: unionOfInvariants  f lAut, 
                                    unionOfClocks f lAut, unionOfGlobalClocks f lAut ,   unionOfCommitted f lAut, 
                                    unionOfVariables f lAut, unionOfGlobalVariables f lAut, unionOfProcedures f lAut);;
(****************************************************************************************************)
(*                                                                                                  *)
(*                    External Choice  Automaton                                                    *)
(*                                                                                                  *)
(****************************************************************************************************)
let getAction (TSBAction a,b,c,d) = a;;
let query = "?";;
let bang = "!";;

let rec ec_generateAllTheBranches (Loc l)  lAut = match lAut with 
[] -> []
| (TSBAction a, TSBExtGuard g, r,  (TimedAutoma (name, locations, init, labels, edges, invariants, 
       clocks, globalClocks,  committed, variables, globalVariables,  procedures))):: tl 
   ->  Edge ( Loc l, Label ( a^query), extGuardToString  g, r , init) :: ec_generateAllTheBranches (Loc l) tl ;;

let rec getLabelsFromEdges l = match l with 
[] -> []
|  Edge (a,lab,c,d,e)::tl -> lab :: getLabelsFromEdges tl;;
 


(*lAut is a tuple ( a, g, r, aut) list*)
let externalChoiceAutomaton (Loc l)  g lAut  = 
               try
                    let edgs =   ec_generateAllTheBranches ( Loc l ) lAut in 
                    (*eliminating query char*) 
                    let labs = List.map (fun (Label x) -> Label (if String.length x > 0 then String.sub x 0  (String.length x -1 ) else "")) (getLabelsFromEdges edgs) in  
                    let f =  (fun (a,b,c,d) -> d) in 
                    let inv =  (l, g) 
                    in TimedAutoma ("", Loc l :: unionOfLocations f lAut, Loc l , addSetSet labs (unionOfLabels f lAut), 
                                    edgs@ unionOfEdges f lAut, inv :: unionOfInvariants f lAut, 
                                    unionOfClocks f lAut, unionOfGlobalClocks f lAut ,   unionOfCommitted f lAut, 
                                    unionOfVariables f lAut, unionOfGlobalVariables f lAut, unionOfProcedures f lAut)
                with 
                   _-> failwith ("Errore qui: externalChoiceAutomaton.") 
;;


(****************************************************************************************************)
(*                                                                                                  *)
(*                    Union of automataton                                                          *)
(*                                                                                                  *)
(****************************************************************************************************)

(*Functions to collapse together all the fields  of all the automata in the  list*)
let sumLocations_a ta_list = eliminateDuplicates(List.flatten (List.map getLocations  ta_list));;
let sumLabels_a ta_list = eliminateDuplicates(List.flatten (List.map getLabels  ta_list));;
let sumEdges_a ta_list = eliminateDuplicates(List.flatten (List.map getEdges  ta_list));;
let sumInvs_a ta_list = eliminateDuplicates(List.flatten (List.map getInvariants  ta_list));;
let sumClocks_a ta_list = eliminateDuplicates(List.flatten (List.map getClocks  ta_list));;
let sumGlobalClocks_a ta_list = eliminateDuplicates(List.flatten (List.map getGlobalClocks  ta_list));;
let sumVariables_a ta_list = eliminateDuplicates(List.flatten (List.map getVariables  ta_list));;
let sumGlobalVariables_a ta_list = eliminateDuplicates(List.flatten (List.map getGlobalVariables  ta_list));;
let sumCom_a ta_list = eliminateDuplicates(List.flatten (List.map getCommitted  ta_list));;
let sumProcs_a ta_list = eliminateDuplicates(List.flatten (List.map getProcs  ta_list));;

let sumAutomata (Loc l)  lAut =  TimedAutoma ("", sumLocations_a lAut, Loc l , sumLabels_a lAut, sumEdges_a lAut, sumInvs_a lAut, 
                                    sumClocks_a lAut, sumGlobalClocks_a lAut , sumCom_a lAut, 
                                    sumVariables_a lAut, sumGlobalVariables_a lAut, sumProcs_a lAut);;



(****************************************************************************************************)
(*                                                                                                  *)
(*                    Mapping tsb into automaton                                                    *)
(*                                                                                                  *)
(****************************************************************************************************)
(****************************************************************************************************)
(*                                                                                                  *)
(*                    Managing reset set                                                            *)
(*                                                                                                  *)
(****************************************************************************************************)
(* Unfortunately UPPAAL doesnot allows for more than a single  resets on an edge, *)
(* unless they are written in a procedure*)
(* so we create a procedure (with name and body) if there is more han one clock, *)
(*and the procedure name goes on the edge*)
(* otherwise we put the clock on the edge with the string t = 0 *)

(* Create createProcFromReset creates the procedure body*)
let createProcFromReset    l = 
       List.fold_right (fun (TSBClock c)  y -> c^"=0; "^y) l "" ;; 

(* Create ResetBody is used if only one clock is to be reset, and returns the string to be put on the edge*)
let createResetStringForEdge  l = match l with  
   [] -> ""
| (TSBClock c)::tl -> c^"=0"
;;

(* manageResetSet returns a couple: if reset set is more than 1 we have (procedure name , (proc Name + procedure body))*)
(* if it is only one we have (string to put on the edge, nothing) *)
(* either way the first item is to be put on the edge, the second --if not empty -- among the procedures*)
let procHD = "res_";;
let manageResetSet r rv = 
       let a = if List.length r >1 then procHD^rv^"()" 
             else (if List.length r = 1 then createResetStringForEdge r else "")  
       in let b = if List.length r >1 then (createProcFromReset r) else "" 
       in (a, if List.length r >1 then [(a,b)] else [])
;; 

let addProcedure  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, 
                   globalClocks,  committed, variables, globalVariables,  procedures))      p = 
                     (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, 
                           globalClocks,  committed, variables, globalVariables,  p @procedures));;

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Mapping tsb into automaton                                                    *)
(*                                                                                                  *)
(****************************************************************************************************)
let addClocks  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, 
                   globalClocks,  committed, variables, globalVariables,  procedures))      c = 
                     (TimedAutoma (name, locations, init, labels, edges, invariants,  addSetSet c clocks, 
                           globalClocks,  committed, variables, globalVariables,  procedures));;

(*clocks for tsb and automata have different constructor*)
let getClocksList  rl = List.map (fun (TSBClock c)  -> Clock c) rl;;

let tau = "t";;

let rec generateInternalChoiceContinuation l = match l with 
[] ->  []
| (TSBAction a, TSBExtGuard g, TSBReset r, rv):: tl ->     let newClocks = addSetSet ( List.map  (fun c  -> Clock c) (getClockListFromGuard g)) (getClocksList r) in
                                               let (n,b) =  manageResetSet r rv in 
                                               let aut = prefixAutomaton (Loc (a^rv)) (Label ( a^bang))  n (idleAutomaton (Loc rv))
                                               in  ( TSBExtGuard  g,  (addClocks  (addProcedure aut b)) newClocks)
                                                    :: generateInternalChoiceContinuation tl
;;

let rec generateExternalChoiceContinuation l = match l with 
[] -> []
| (TSBAction a, TSBExtGuard g, TSBReset r, rv):: tl ->   let newClocks = addSetSet ( List.map  (fun c  -> Clock c) (getClockListFromGuard g)) (getClocksList r) in
                                               let (n,b) =  manageResetSet r rv in 
                                               let aut =  (idleAutomaton (Loc rv))
                                               in  (TSBAction a, TSBExtGuard  g, n,   (addClocks  (addProcedure aut b)) newClocks)
                                                    :: generateExternalChoiceContinuation tl
;;

let denfToUppaal (x,p) = match p with   
   DESuccess ->  successAutomaton (Loc x) 
|  DEIntChoice  l -> let lAut = generateInternalChoiceContinuation l in
                     let omega = getOmega l  
                     in prefixAutomaton  (Loc (x)) (Label "") "" (internalChoiceAutomaton (Loc (tau^x)) omega  lAut) 
|  DEExtChoice  l -> let lAut = generateExternalChoiceContinuation  l in
                     let omega = ""  
                     in externalChoiceAutomaton (Loc x) omega  lAut 
| _ ->  failwith "denfToUppaal Error 2";;



let rec buildAutomaton (x,d) =  let lAut = List.map (fun a -> denfToUppaal a) d
                                in sumAutomata (Loc x) lAut;;


(*buildAutomaMain is the main methode to convert a ExtTSBprocess into an Uppaal automata*)
(*it needs the process p and a name/identifier for it*)
(*First we transform a process into a denf AND we normalize all the guards*)
let buildAutomatonMain t name = let (x, d)  = extTsbToDENF t  
                             in  let uppAut = buildAutomaton (x,d)
                             in  setName uppAut name
;;

(*extTsb_mapping performs the conversion of two ExtTSBprocesses p and q into two Uppaal automata*)
(*"p" and "q" are two simple names, used only in UPPAAL*) 
let extTsb_mapping p q  =  [ buildAutomatonMain p "p" ; buildAutomatonMain q "q"] ;;

(*tsb_mapping performs the conversion of two TSBprocesses p and q into two Uppaal automata*)
(*"p" and "q" are two simple names, used only in UPPAAL*) 
let tsb_mapping p q  =  [ buildAutomatonMain ( toExtTsb  p) "p" ; buildAutomatonMain ( toExtTsb  q) "q"] ;;


(*DEBUG TESTS********************************************************************************************************)

let r1 = [TSBClock "x";TSBClock "t"];;
let r2 = [TSBClock "t"];;
let g1 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,6), (SC(TSBClock "x",ExtLessEq,12))));;
let g2 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,10), (SC(TSBClock "x",ExtLessEq,22))));;
let t  = ExtIntChoice [(TSBAction "c", g1, TSBReset r1, ExtSuccess); 
                       (TSBAction "d", g2, TSBReset r2, ExtSuccess)];;
buildAutomatonMain t "p";;

let q  = ExtExtChoice [(TSBAction "c", g1, TSBReset r1, ExtSuccess); 
                       (TSBAction "d", g2, TSBReset r2, ExtSuccess)];;

buildAutomatonMain q "p";;


