
#use "tipi.ml";;
#load "str.cma";;


(*****************Checking time constraint for the enabling************************)
(*                                                                                *)
(*As soon as events happen, we check if they are in time, in relation with the time *)
(*constraint of the enabling. A single event may occurr in different enablings, so it may be in time *)
(*for one, and not in time for an other.                                          *)
(*Every time-constraint has time boundaries:  an inferior (N) and a superior (S) bound *)                                                     (*boundary may be  absolute or related to an other event. *)
(*Superior bound may be infinite, which means no boundary.                         *)
(*                                                                                *)
(**********************************************************************************) 
let isRelativeTC (e1,t1) = match e1 with
         None -> false
       | _-> true;; 

let isInfinityTC (e1,t1) = match t1 with
         Infinity -> true
       | _-> false;; 

let getInfBound  (TimeConstraint((e1,t1), e, (e2,t2))) = (e1,t1);;
let getSupBound  (TimeConstraint((e1,t1), e, (e2,t2))) = (e2,t2);;
 
let normalize  (a,b) = match (a,b) with
         (e1, TimeB t1) -> (e1, t1)
        | _ -> failwith "Normalize ";; 

(*diseguality depends from boundary: if inferior boundary or superior one*)
let calculateDiseguality w = if w = "N" then  (">=", "<") else ("<", ">=");;

let calculateLabel  lab =   (lab, "!"^lab)  ;;

(*The function buildRelativeTCInf builds the inferior check for a relative time constraint*)
(* w is a string to compose the name of nodes. For inferior bound is N, for superior is S*)
(* succ is the successive location *)
let buildRelativeTCInf w x (Event e) tc   succ
          (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = match tc with 
    (RelEvent ev, n)-> (
        let newLocs =  addElSet (Loc ("K"^w^e^"_"^x^"0"))  (addElSet (Loc ("K"^w^e^"_"^x^"1")) (addElSet (Loc  ("K"^w^e^"_"^x^"2")) locs)) in
        let (diseg, notDiseg) = calculateDiseguality w in
        let (label, notLabel) = calculateLabel  (ev^"Fired") in
        let newEdges =  addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", label, "", Loc ("K"^w^e^"_"^x^"1")))
                       (addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", notLabel, "", Loc ("KK"^e^"_"^x^"1")))
                       (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^diseg^(string_of_int n), "", Loc ("K"^w^e^"_"^x^"2")))
                       (addElSet (Edge (Loc ("K"^w^e^"_"^x^"2"), Label "", "", "", succ)) (*collegamento col succesivo*)
                       (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^notDiseg^(string_of_int n), "", Loc ("KK"^e^"_"^x^"1")))  edges)))) in
        let newCommitted  = setCommitted (setCommitted  (setCommitted  committed  (Loc ("K"^w^e^"_"^x^"0")))  (Loc ("K"^w^e^"_"^x^"1")))  (Loc ("K"^w^e^"_"^x^"2"))  
        in ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars, procs))
    )
    | _ ->  failwith "buildRelativeTC uncorrect syntax";;

(*The function buildRelativeTCSup builds the superior check for a relative time constraint*)
let buildRelativeTCSup w x (Event e) tc   succ
            (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = match tc with 
    (RelEvent ev, n)-> (
        let newLocs =  addElSet (Loc ("K"^w^e^"_"^x^"0")) (addElSet (Loc ("K"^w^e^"_"^x^"1")) (addElSet (Loc  ("K"^w^e^"_"^x^"2")) locs)) in
        let (diseg, notDiseg) = calculateDiseguality w in
        let (label, notLabel) = calculateLabel  (ev^"Fired") in
        let newEdges = addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", label, "", Loc ("K"^w^e^"_"^x^"1")))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", notLabel, "", Loc ("KK"^e^"_"^x^"0")))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^diseg^(string_of_int n), "", Loc ("K"^w^e^"_"^x^"2")))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"2"), Label "", "", "", succ)) (*collegamento col succesivo*)
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^notDiseg^(string_of_int n), "", Loc ("KK"^e^"_"^x^"1")))  edges)))) in
        let newCommitted  = setCommitted (setCommitted  (setCommitted  committed  (Loc ("K"^w^e^"_"^x^"0")))  (Loc ("K"^w^e^"_"^x^"1")))  (Loc ("K"^w^e^"_"^x^"2"))  
        in ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars, procs))
    )
    | _ ->  failwith "buildRelativeTC uncorrect syntax"
                ;;
(*general*)
let buildRelativeTC w x (Event e) tc   succ
            (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = match tc with 
    (RelEvent ev, n)-> (
        let newLocs =  addElSet (Loc ("K"^w^e^"_"^x^"0")) (addElSet (Loc ("K"^w^e^"_"^x^"1")) (addElSet (Loc  ("K"^w^e^"_"^x^"2")) locs)) in
        let (diseg, notDiseg) = calculateDiseguality w in
        let (label, notLabel) = calculateLabel  (ev^"Fired") in
        let loc = if w = "S" then Loc ("KK"^e^"_"^x^"0") else Loc ("KK"^e^"_"^x^"1") in  
        let newEdges = addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", label, "", Loc ("K"^w^e^"_"^x^"1")))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", notLabel, "", loc ))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^diseg^(string_of_int n), "", Loc ("K"^w^e^"_"^x^"2")))
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"2"), Label "", "", "", succ)) (*link to followers*)
                      (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", ev^"C"^notDiseg^(string_of_int n), "", Loc ("KK"^e^"_"^x^"1")))  edges)))) in
        let newCommitted  = setCommitted (setCommitted  (setCommitted  committed  (Loc ("K"^w^e^"_"^x^"0")))  (Loc ("K"^w^e^"_"^x^"1")))  (Loc ("K"^w^e^"_"^x^"2"))  
        in ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars, procs))
    )
    | _ ->  failwith "buildRelativeTC uncorrect syntax";;

(*let buildRelativeTC w x = if w = "N" then buildRelativeTCInf w x else buildRelativeTCSup w x;; *)

let buildAbsoluteTC w x (Event e) tc  succ  
       (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) =  match tc with 
    (None, n) ->(
        let newLocs =  addElSet (Loc ("K"^w^e^"_"^x^"0")) (addElSet (Loc ("K"^w^e^"_"^x^"1"))  locs) in
        let (diseg, notDiseg) = calculateDiseguality w in
        let newEdges = addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", "x"^diseg^(string_of_int n), "", Loc ("K"^w^e^"_"^x^"1")))
                           (addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", "x"^notDiseg^(string_of_int n), "", Loc ("KK"^e^"_"^x^"1")))
                           (addElSet (Edge (Loc ("K"^w^e^"_"^x^"1"), Label "", "", "", succ)) edges)) in (*link to followers*)
        let newCommitted  = setCommitted  (setCommitted  committed  (Loc ("K"^w^e^"_"^x^"0")))  (Loc ("K"^w^e^"_"^x^"1"))  
        in ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars, procs))
    )
    | _ -> failwith "buildAbsoluteTC uncorrect syntax";;


let buildInfinityTC  w x (Event e) tc (Loc succ)  
         (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = 
    let newLocs =  addElSet (Loc ("K"^w^e^"_"^x^"0"))  locs in
    let newEdges = addElSet (Edge (Loc ("K"^w^e^"_"^x^"0"), Label "", "", "",  (Loc ("KK"^e^"_"^x^"0")))) edges in (*link to followers*)
    let newCommitted  = setCommitted  committed  (Loc ("K"^w^e^"_"^x^"0"))  
    in ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars, procs))
                ;;

(*The functioncheckConstraints checks if constraints specified in enablings have been satisfied  when events were fired*)
(*At least one time constraint per event is allowed in the enabling *)
let rec checkConstraintsRec (Event e) ens_e last (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) =  match ens_e with 
      [] ->  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs))
    | Enabling(evSet, ev, tc,  a, b)::tl ->  
        let x = enablingToString (Enabling(evSet, ev, tc, a, b)) in  
        let timeConstr_e = List.filter (fun (TimeConstraint(t1,Event ev,t2)) -> if ev = e then true else false )  tc in
        let n =  List.length timeConstr_e in 
        if (n>1) then failwith ("Too many time-constraints for event "^e^" in enabling "^x)
        else( 
            let newVars = addElSet (e^"InTime_"^x) ( addElSet (x^"Fired_"^x)  vars)  in 
            let newLocs =  addElSet (Loc ("KK"^e^"_"^x^"0")) (addElSet (Loc ("KK"^e^"_"^x^"1")) locs) in 
            let newCommitted  = setCommitted  (setCommitted  committed  (Loc ("KK"^e^"_"^x^"0")))  (Loc ("KK"^e^"_"^x^"1"))  in
            let newEdges = addElSet (Edge (Loc ("KK"^e^"_"^x^"0"), Label "",  "", e^"InTime_"^x^"=true",   Loc ("KK"^e^"_"^x^"1"))) 
                                    (addElSet (Edge (Loc ("KK"^e^"_"^x^"1"), Label "", "", "", last)) edges)  in
            let newEdges = if (List.length tl = 0) then addElSet (Edge (Loc ("S"^e^"0"), Label "", "", "", Loc ("KN"^e^"_"^x^"0"))) newEdges else newEdges in
            if n=0 then (  (*no time constraints on this event in this enabling*) 
                let newLocs =  addElSet (Loc ("KN"^e^"_"^x^"0")) newLocs in 
                let newCommitted  = setCommitted  newCommitted  (Loc ("KN"^e^"_"^x^"0"))   in 
                let newEdges = addElSet (Edge (Loc ("KN"^e^"_"^x^"0"), Label "", "", "", Loc ("KK"^e^"_"^x^"0"))) newEdges    in
                       checkConstraintsRec (Event e) tl  (Loc ("KN"^e^"_"^x^"0")) 
                                  ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks,  newCommitted, newVars, gVars, procs))
            ) 
            else( (* first of all, we do some general stuff...  *)
                (*then we go on with constraints: theOne is the right and only  time constraints for e in  evSet*)
                let theOne = List.nth  timeConstr_e 0 in
                let succ = (Loc("KS"^e^"_"^x^"0")) in 
                (*for inferior bound  we must decide if it is relative or absolute *)
                let aut = if (isRelativeTC (getInfBound theOne)) then buildRelativeTC "N" x (Event e) (getInfBound theOne) succ
                                  (TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, newVars, gVars, procs))
                          else buildAbsoluteTC "N" x  (Event e) (getInfBound theOne) succ
                                  (TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, newVars, gVars, procs))
                in(*for  superior bound we must decide if it is relative absolute or infinity*)
                let succ =  Loc ("KK"^e^"_"^x^"0") in 
                let aut = if  isInfinityTC (getSupBound theOne)  then  buildInfinityTC  "S" x  (Event e) (getInfBound theOne)  succ  aut
                          else (if isRelativeTC (getSupBound theOne) then (buildRelativeTC "S" x  (Event e) (normalize (getSupBound theOne)) succ aut)
                                else   buildAbsoluteTC "S" x  (Event e) (normalize (getSupBound theOne)) succ  aut
                          ) 
                in  checkConstraintsRec (Event e) tl  (Loc("KN"^e^"_"^x^"0")) aut
            )
        );;
           

let  checkConstraints (Event e) ens_e last (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) = 
if (List.length ens_e = 0 ) then 
       let newEdges = addElSet (Edge (Loc ("S"^e^"0"), Label "", "", "", Loc ("S"^e^"1"))) edges 
       in  ( TimedAutoma (name, locs, initial, labels, newEdges,  inv,  clocks, gClocks, committed, vars, gVars, procs))
else checkConstraintsRec (Event e) ens_e last (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs));;


(*********Checking which enabling was in time ***********)
(*                                                      *)
(*The function manageFired manage the flow in case a whole enabling has been completely fired*)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let rec manageFired (Event e) ens_e  
         (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars,  procs)) = match ens_e with 
      [] ->  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs))
    | Enabling(evSet,ev,tc, a, b)::tl ->  let x = enablingToString (Enabling(evSet,ev, tc, a, b)) in  
        let newEdges = addElSet (Edge (Loc ("S"^e^"2"), Label "", x^"Fired_"^x, x^"C=0", Loc ("K"^x^"0"))) edges 
        in manageFired (Event e) tl ( TimedAutoma (name, locs, initial, labels, newEdges,  inv,  clocks, gClocks,  committed, vars, gVars,  procs));;


let rec firingGuard ens_e = match ens_e with 
       [] -> ""
    | Enabling(evSet,ev,tc, a,b)::hd2::tl ->  let x = enablingToString (Enabling(evSet,ev,tc,a,b)) in "!"^x^"Fired_"^x^" && "^(firingGuard (hd2::tl))
    | Enabling(evSet,ev,tc, a,b)::tl ->  let x = enablingToString (Enabling(evSet,ev,tc, a,b)) in "!"^x^"Fired_"^x^(firingGuard tl);;


(*************************Listening for events to happen********************)
(*                                                                         *)
(* The function listen ens allEvn iterates on all the events in allEvn and for each of them *)
(* filters all the enables in ens which contains the selected event.        *)
(* On that event, it creates a set of locations which listens to that event to happen *)
(* and performs some checks on enabling and timing                          *)
(*                                                                          *)
(*The function  createDoneProcedure builds a string like this:              *)
(*void aDone(){                                                             *)
(*abFired_ab = aFired &amp;&amp; bFired;                                    *)
(*abInTime_ab = aInTime_ab &amp;&amp; bInTime_ab;                           *)
(*                                                                          *)
(*acdFired_acd = aFired &amp;&amp; cFired &amp;&amp; dFired;                *)
(*acdInTime_acd = aInTime_acd &amp;&amp; cInTime_acd &amp;&amp; dInTime_acd;*)
(*}                                                                         *)
(****************************************************************************) 
let rec createFired  evs  = match evs with 
       [] -> ""
    | Event e::hd2::tl -> e^"Fired && "^(createFired (hd2::tl))
    | Event e::[] -> e^"Fired;"
   ;;

let rec createInTime   x evs  = match evs with 
       [] -> ""
    | Event e::hd2::tl -> e^"InTime_"^x^" && "^(createInTime x (hd2::tl))
    | Event e::[] -> e^"InTime_"^x^";";
   ;;

let rec createDoneProcedures  ens_e  = match ens_e with 
      [] -> ""
    | Enabling(evSet,ev,tc, a,b)::tl ->  let x = enablingToString (Enabling(evSet,ev,tc,a,b)) 
        in x^"Fired_"^x^" = "^(createFired evSet)^"\n"^x^"InTime_"^x^" = "^(createInTime  x evSet)^"\n\n"^(createDoneProcedures tl) ;;
 
let rec createShotProcedure  e  =  e^"Fired = true; \n"^e^"C = 0;\n\n";; 

(*The function listen waits for the enabling events to happen and when they do, it checks their time constraints. Ens are  the enablings for the single event e, Allevents are all the flatten enabling  *)
let rec  listen ens allEvn 
         (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = match allEvn with 
    [] -> (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars,  procs))
   |(Event e)::tl ->  let ens_e = List.filter (fun (Enabling (x,y,tc,a,b)) -> if (List.mem (Event e) x) then true else false) ens in 
        let newProcs = addElSet (e^"Done()", createDoneProcedures ens_e) procs in 
        let newLocs =  addElSet (Loc ("S"^e^"2"))  (addElSet (Loc ("S"^e^"1"))  (addElSet (Loc ("S"^e^"0")) locs)) in
        let newCommitted = setCommitted (setCommitted  (setCommitted  committed (Loc  ("S"^e^"2"))) (Loc  ("S"^e^"1"))) (Loc  ("S"^e^"0")) in
        let newEdges = addElSet (Edge (Loc ("L0"), Label (e^"?"), "", "", Loc ("S"^e^"0")))
                      (addElSet (Edge (Loc ("S"^e^"2"), Label "", (firingGuard ens_e), "", Loc ("L0")))
                      (addElSet (Edge (Loc ("S"^e^"1"), Label "", "", e^"Done()", Loc ("S"^e^"2"))) edges))
        in  checkConstraints (Event e) ens_e (Loc ("S"^e^"1")) (manageFired (Event e) ens_e 
                      (listen ens tl (TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks, newCommitted, vars, gVars,  newProcs))));;

      
(********************************************************)
(*                                                      *)
(*The function checkIsInTime check if the enabling has been fired in time, *)
(*according with its constraints. In the case the enabling is empty, we skip the check*)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let rec checkIsInTime  (Enabling(evSet,ev,tc,a,b))  
          (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) =  
    let x = enablingToString (Enabling(evSet,ev,tc,a,b)) in 
    if (x="") then  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars,  procs))
    else( let newLocs =  addElSet (Loc ("K"^x^"0")) (addElSet (Loc ("K"^x^"1")) locs) in
        let newVars = addElSet (x^"InTime_"^x) vars in 
        let newCommitted  = bind committed (Loc  ("K"^x^"0")) true in
        let newEdges = addElSet (Edge (Loc ("K"^x^"0"), Label "", "!"^x^"InTime_"^x, "", Loc ("K"^x^"1")))
                      (addElSet (Edge (Loc ("K"^x^"0"), Label "", x^"InTime_"^x, "", Loc ("W"^x^"0"))) edges) 
        in  (TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks,  newCommitted, newVars, gVars, procs)));;


(********************************************************)
(*                                                      *)
(*The function waitToFireRec create the waiting section for *)
(* the given enabling. For instance in  the case the event has to wait from 3 to 5 before being fired*)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let  waitToFire   (Enabling(evSet,ev,tc,a,b)) (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) = 
    let x = enablingToString (Enabling(evSet,ev,tc,a,b)) in 
    let newLocs =  addElSet (Loc ("W"^x^"0")) (addElSet (Loc ("W"^x^"1")) locs) in
    let newClocks = addElSet (Clock (x^"C")) clocks in
    let newEdges = addElSet (Edge (Loc ("W"^x^"1"), Label "",  "", "", Loc ("F0")))
            (addElSet (Edge (Loc ("W"^x^"0"), Label "",  x^"C =="^(string_of_int a), "", Loc ("W"^x^"1"))) edges) in
    let newInv = bind (bind inv  ("W"^x^"0")  ((x^"C")^"<="^(string_of_int a)) ) ("W"^x^"1")  ((x^"C")^"<"^(string_of_int b))
    in   (TimedAutoma (name, newLocs, initial, labels, newEdges,  newInv,  newClocks, gClocks, committed, vars, gVars, procs));;


(********************************************************)
(*                                                      *)
(*The function buildEnRec iterates on all the enablings and for each of them, *)
(* check if they were performed in time and then wait to fire the event, at the right time window*)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let rec buildEnRec  ens (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) =  match ens with
    [] -> (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs))
   | hd::tl -> buildEnRec tl (checkIsInTime hd (waitToFire hd (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)))) ;;


(********************************************************)
(*                                                      *)
(*To actually fire the event e, we listen for e-enabling-events to happen in time, *)
(*then we fire. If no enabling event is present, we just fire.**)                                                     
(*Since we use stable event structure, if there is an empty enabling*)
(*then it must be the only one.                                                     *)
(*If the enabling events happens not in time, then e is not doomed to happen                    *)
(********************************************************) 
let prepareToFire  (Event e) (TimedEventStructure (events, enablings, conflicts)) (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) =
    let ens = List.filter (fun (Enabling (evSet, Event ev, tc, a,b))-> if (ev = e) then true else false) enablings  in (*ens are the enblings for e*)
    let empty_ens = List.filter (fun  (Enabling (evSet, Event ev, tc, a,b)) -> if evSet = [] then true else false ) ens (*we search for the empty enabling*)
    in if (List.length empty_ens = 1) then (
        if  (List.length ens > 1 ) then failwith ("Error: Not a stable timed event structure: the empty enabling for event "^e^" is not the only one")
        else ( (*empty enabling set*)
            let newEdges = addElSet (Edge (Loc ("L0"), Label "",  "", "", Loc ("W0"))) edges in
            let newCommitted = setCommitted committed (Loc ("L0")) 
            in (buildEnRec ens  (TimedAutoma (name, locs, initial, labels, newEdges,  inv,  clocks, gClocks,  newCommitted, vars, gVars,  procs)))
            ))
    else (*no empty enabling*)  listen ens (flattenEnablings ens) (buildEnRec ens  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)));;
     
(********************************************************)
(*                                                      *)
(*Before actually firing the event e, we must check that no*)
(* other conflicted event has happenend .                *)
(*This is done in function checkConflicts               *)
(*                                                      *)
(*                                                      *)
(********************************************************) 

let rec checkConflictsRec (Event e) cfl_e last 
              (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars,  procs)) =  match cfl_e with 
    [] ->  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars,  procs))
   |Conflict(Event ev1,Event ev2)::tl ->   
        let x = if (ev1 = e) then ev2 else ev1 in
        let newLocs =  addElSet (Loc ("F"^x^"0")) (addElSet (Loc ("F"^x^"1")) locs)  in
        let newEdges = addElSet (Edge (Loc ("F"^x^"0"), Label "", x^"Fired", "", Loc ("F"^x^"1")))
                       (addElSet (Edge (Loc ("F"^x^"0"), Label "", "!"^x^"Fired", "", last)) edges) in 
        let newEdges = if (List.length tl = 0) then addElSet (Edge (Loc ("F0"), Label "", "", "", Loc ("F"^x^"0"))) newEdges else newEdges in
        let newCommitted  = setCommitted  committed  (Loc ("F"^x^"0"))  
        in checkConflictsRec  (Event e) tl  (Loc ("F"^x^"0")) ( TimedAutoma (name, newLocs, initial, labels, newEdges,  inv,  clocks, gClocks,  newCommitted, vars, gVars,  procs));;      


(*If there are no conflicts we just draw an edge from F0 to the firing location F1, otherwise, we build up conflict-check locations*)
let  checkConflicts (Event e) cfl_e last 
               (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = 
    if List.length cfl_e == 0 then 
        let newEdges = addElSet (Edge (Loc ("F0"), Label "", "", "", Loc ("F1"))) edges 
        in  ( TimedAutoma (name, locs, initial, labels, newEdges,  inv,  clocks, gClocks,  committed, vars, gVars,  procs))
    else  checkConflictsRec (Event e) cfl_e last (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs));;


(********************************************************)
(*                                                      *)
(*The function firingEvent add the last two locations F and Fired    *)
(*to the automa, plus the edge which actually fires the event e  ,   *)
(*this means that the label e! is sent on  the broadcast channel e   *)
(*To have two reset actions on the edge, we must collect these action on  a procedure: eShot              *)
(********************************************************) 
let firingEvent (Event e) (TimedEventStructure (events, enablings, conflicts))  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs))  =
    let newLocs = addElSet (Loc "F0") ( addElSet (Loc "F1") (addElSet (Loc "Fired") locs))  (*adding locations F0 and Fired*)
    and  newEdges = addElSet (Edge (Loc "F1", Label (e^"!"), "", e^"Shot()", Loc "Fired")) edges (*firing e!*)
    and  newCommitted  = setCommitted ( setCommitted committed (Loc "F1")) (Loc "F0")          
    and  newProcs =  (addElSet (e^"Shot()", createShotProcedure e) procs) 
    in prepareToFire (Event e) (TimedEventStructure (events, enablings, conflicts)) 
                  (checkConflicts (Event e)  
                    (List.filter (fun (Conflict(Event ev1, Event ev2))-> if (ev1 = e || ev2 = e) then true else false) conflicts) (Loc "F1")  
                        (TimedAutoma (name, newLocs, initial, labels, newEdges, inv, clocks, gClocks, newCommitted, vars, gVars, newProcs)));;
 
(********************************************************)
(*                                                      *)          
(*The function "buildAutoma tes e " returns  automa for the event e. *)
(*The construction is bottom-up.                      ****)
(**Every automa uses some global variables and global clocks . ***)
(*                                                      *)
(*                                                      *)
(********************************************************)      
let buildAutoma  (TimedEventStructure (events, enablings, conflicts))  (Event e)  =  
    let locs =   [Loc "L0"] (*initial location*)
    and clocks =  [] 
    and gClocks =  (Clock "x") :: (List.map (fun (Event e) -> (Clock (e^"C")))  events) (*shared clocks linked to events*)
    and committed = fun loc -> false 
    and globalVars =  (List.map (fun (Event e) -> e^"Fired")  events)(*shared variables linked to events*)
    and vars = []       
    and labels = List.map (fun (Event e) -> Label e)  events (*all the possible labels are the one generated by events*)
    and procs = []
    in  firingEvent (Event e) (TimedEventStructure (events, enablings, conflicts))  (TimedAutoma (e, locs, Loc "L0",  labels,  [], emptyInv,  clocks, gClocks, committed, vars, globalVars, procs));;


(*The function "getEnabledEvents" returns all the events for which there exists (at least) an enabling *)
let rec getEnabledEvents  en = match en with 
  [] -> []  
| Enabling( evs, ev, tc, a,b)::tl->  addElSet ev (getEnabledEvents tl)  ;;
 

(*The function "mapping" return a network of automata since every enabled event is mapped into a single  automa*)
let es_mapping (TimedEventStructure (events,  en, conflicts)) =   
         List.map (buildAutoma  (TimedEventStructure (events, en, conflicts)))  ( getEnabledEvents en);;

     

