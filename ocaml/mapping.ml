
(*#use "tipi.ml";;*)
(*#load "str.cma";;*)

open Tipi;;

(*getter functions for  [(Action "a", g, r , Success)*)
let getAction (TSBAction a,b,c,d) = a;;
let getGuard (a,TSBGuard b,c,d) = b;;
let getReset (a,b,TSBReset c,d) = c;;
let getSuffix (a,b,c, d) = d;;

(*Functions to collapse together all the fields  of all the automata in the  list*)
let sumLocations_a ta_list = eliminateDuplicates(List.flatten (List.map getLocations  ta_list));;
let sumLabels_a ta_list = eliminateDuplicates(List.flatten (List.map getLabels  ta_list));;
let sumEdges_a ta_list = eliminateDuplicates(List.flatten (List.map getEdges  ta_list));;
let sumProcs_a ta_list = eliminateDuplicates(List.flatten (List.map getProcs  ta_list));;
let sumClocks_a ta_list = eliminateDuplicates(List.flatten (List.map getClocks  ta_list));;
let sumInvs_a ta_list = eliminateDuplicates(List.flatten (List.map getInvariants  ta_list));;
let sumCom_a ta_list = eliminateDuplicates(List.flatten (List.map getCommitted  ta_list));;

(*Used to compone the labels*)
let bar = "bar_";;
let bang = "!";;
let query = "?";;
let procHD = "res_";;


(*Converter for guards: from   [(TSBClock "t", Less, 10); (TSBClock "t", Great, 1)] to "t<10 && t>1"*)
let getRelation r = match r with 
  Less -> "<"
| Great -> ">";;

let getClockName (TSBClock c) = c;;

let getClocksListFromGuards gl = List.fold_right (fun (TSBClock c, b, d)  y -> (Clock c)::y) gl [];;

let rec tsbGuardToString gl = match gl with 
    [] -> ""
| (c,r,d)::tl1::tl2-> getClockName(c)^getRelation(r)^(string_of_int d)^" && "^(tsbGuardToString (tl1::tl2))
| (c,r,d)::tl1  ->   getClockName(c)^getRelation(r)^(string_of_int d)
;;


(****************************************************************************************************)
(*                                                                                                  *)
(*                    Edges and procedures for resets                                               *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)

(*Unfortunately UPPAAL doesnot allows for more resets on an edge, unless they are written in a procedure*)
(*createResetProc create the reset procedure and returns also the clock list*)
let createResetProc  name  l = 
   [(name,  List.fold_right (fun (TSBClock c)  y -> c^"=0; "^y) l "" )],  
     List.map (fun (TSBClock c)  -> Clock c) l;; (*the clock list*)

(* let createResetProc  name  l = match l with  *)
(*    [] -> ([],[]) *)
(* | (TSBClock c)::tl -> let  (procs, clocks) =  createResetProc *)
(* ;; *)


(* CreateIntEdges create the list of new edges for an internal choice.*)
(* createIntEdges: given init initial location , l a list of  [(Action "a", g, r , Success);*)
(* tadl is the list of  automata in the suffix, and an index *)
(* it returns the list of new edges, last used index new, list of procedures and list of clocks*)
let rec createIntEdges  init l  tadl idx  = match (l, tadl) with (*idx has already been used*)
   [],[] -> ([], idx, [], [],  [], [])
| (hd::tl, hd2::tl2) ->  let (edges_a, last_used_idx, proc_a, clockList_a, invariant_a, committed_a) = createIntEdges init tl tl2 (idx) in
       let current = string_of_int (last_used_idx+1)  in
       (* creating a reset procedure only if there is some clock to reset*)
       let procName =if List.length(getReset hd)>0 then procHD^"l1_"^current^"()" else "" in 
       let (proc, clocksInReset) = if List.length(getReset hd)>0 then createResetProc procName (getReset hd) else ([],[]) in 
       let clocksInGuards = getClocksListFromGuards (getGuard hd) in 
       let clocks  = clocksInGuards @ clocksInReset @ clockList_a in 
       let edges =  [ Edge (init, Label "", tsbGuardToString(getGuard hd) ,"", Loc ("l1_"^current));
            Edge (Loc ("l1_"^current), Label (bar^(getAction hd)^bang), "", procName, Loc ("l2_"^current));
            Edge (Loc ("l2_"^current), Label ((getAction hd)^query), "","", getInit hd2)] @ edges_a in
       let invariant = []  in
       let committed = [] 
       in (edges, (last_used_idx + 1), proc @ proc_a, clocks, invariant @ invariant_a, committed @ committed_a )
| _ -> failwith "Error in createIntEdges"
;;


(*CreateExtEdges create the list of new edges for an external choice.*)
(* createIntEdges: given init initial location , l a list of  [(Action "a", g, r , Success);*)
(* tadl is the list of  automata in the suffix, and an index *)
(* it returns the list of new edges, last used index new, list of procedures and list of clocks*)
let rec createExtEdges  init l  tadl idx  = match (l, tadl) with (*idx has already been used*)
   [],[] -> ([], idx, [], [], [], [])
| (hd::tl, hd2::tl2) ->  let (edges_a, last_used_idx, proc_a, clockList_a, invariant_a, committed_a) = createExtEdges init tl tl2 (idx) in
       let current = string_of_int (last_used_idx+1)  in
       (* creating a reset procedure only if there is some clock to reset*)
       let procName =if List.length(getReset hd)>0 then procHD^"l1_"^current^"()" else "" in 
       let (proc, clocksInReset) = if List.length(getReset hd)>0 then createResetProc procName (getReset hd) else ([],[]) in 
       let clocksInGuards = getClocksListFromGuards (getGuard hd) in  
       let clocks  = clocksInGuards @ clocksInReset @ clockList_a in
       let edges =  [ Edge (init, Label  (bar^(getAction hd)^query), "","", Loc ("l1_"^current));
            Edge (Loc ("l1_"^current), Label "", tsbGuardToString(getGuard hd),"", Loc ("l2_"^current));
            Edge (Loc ("l2_"^current), Label ((getAction hd)^bang), "", procName, getInit hd2)] @ edges_a   in
       let invariant = [] in
       let committed = ["l1_"^current] 
       in (edges, (last_used_idx + 1), proc @ proc_a, clocks,  invariant @ invariant_a, committed @ committed_a )
| _ -> failwith "Error in createExtEdges"
;;
 
(*CreateRecEdges solves the mapping between name and procedure declaration by adding an edge*)
(*takes name of the procedure and location base and list of locations*)
(*it returns the list of edges and the updated list of rec_locations*)
let rec createRecEdges  name loc list   = match list with (*idx has already been used*)
   [] -> ([], [])
| (currName, currLoc)::tl ->  let (edges_a, currList_a) =  createRecEdges name loc tl 
                              in ((if currName = name then [Edge (currLoc , Label "", "","", loc)] else [])@edges_a , 
                                       (if currName = name then [] else [(currName, currLoc)]) @ currList_a);;                          



(****************************************************************************************************)
(*                                                                                                  *)
(*                    Invariants for internal choice:                                               *)
(*            you can wait until there is something you can do                                      *)
(*                                                                                                  *)
(****************************************************************************************************)
(*supInvariants calculate the maximum moment you can wait before choosing a branch *)
(*We assume they deal with the same clock*)
(* l is:  [ (TSBAction "a", g2, r2 , Success), (TSBAction "a", g3, r2 , Success), (TSBAction "a", g4, r2 , Success) ];;*)
(* g is :  TSBGuard [(TSBClock "t", Less, 10); (TSBClock "t", Great, 1)];;*)
(* in the case t<10 && t < 3 sup Invariant returs (t, 10)*)
(* in the case t>10 sup Invariant returs (t, 0)*)

(*getMaxInc calculate the local maximum for the single guard: x> 5 returns "forever" as invariants*)
(* while x>5 && x <10 return Until  (x, 10)*)
(* the supInvariants takes the maximun which is Forever*)
type bound = Forever | Until of string * int;;


let minBound b1 b2 = match (b1, b2) with 
     (Forever, b2)  -> b2
|    (b1, Forever)  -> b1 
|    (Until (t,d), Until (r,c)) -> 
               if t <> r then failwith ("MinBound: "^t^" "^r^" different clocks in a choice")
               else if d < c then b1 else b2
;;

let maxBound b1 b2 = match (b1, b2) with 
     (Forever, b2)  -> Forever
|    (b1, Forever)  -> Forever
|    (Until (t,d), Until (r,c)) -> 
               if t <> r then failwith ("MaxBound:  "^t^" "^r^" different clocks in a choice")
               else if d < c then b2 else b1
;;


(*for a single action you can have a list of guards t<1 && t>2 are represented ad list*)
(* and you returns the minimun bound since it is an INTERVALL and you take the intersection*)
let rec getBoundOfASingleAction l =  match l with 
  [] -> Forever
| (TSBClock c, r, d)::tl -> let bound =  getBoundOfASingleAction tl in 
                            if r = Great then bound else minBound bound (Until (c,d))
;;
 

let rec getBoundOfAllTheActions l b = match l with 
  [] -> b
| (a, TSBGuard gl, r, e)::tl -> let b1 = getBoundOfASingleAction gl in 
                              let m = maxBound b1 b 
                              in  getBoundOfAllTheActions tl m
;;

let getClock l = match l with 
   [] -> ""
|   (TSBClock c, r, d)::tl -> c;;

let boundToInv b = match b with
  Forever -> ""
| Until (c,d) -> c^" < "^string_of_int d;;

(*to initialize the list with a default value for minimum I need to know the clock name*)
let getMaxInv l =  if List.length l = 0 then ""
                   else  let c = getClock (getGuard (List.nth l 0))
                   in boundToInv(getBoundOfAllTheActions l (Until (c, 1)));;

(****************************************************************************************************)
(*                                                                                                  *)
(*                    Success Automa                                                                *)
(*                                                                                                  *)
(****************************************************************************************************)
(*The success state synchronize on a channel and then performe a self loop, to allow for the property not deadlock to be true*)
let successLoc = Loc "f";;
let successSync = "success";;
let successAutoma = let edges =  [ Edge (successLoc, Label  (successSync^"?"), "","", Loc ("f_0"));
                                   Edge (successLoc, Label  (successSync^"!"), "","", Loc ("f_0"));
                                   Edge ( Loc ("f_0"), Label "", "","", Loc ("f_0"))]
                    in TimedAutoma ("",[successLoc], successLoc,[Label successSync],edges,[], [], [], [], [], [],  []);;
(****************************************************************************************************)
(*                                                                                                  *)
(*                    Normalizing guards                                                            *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)
(*A single guard is composed by a list of clock constraints, which  may contains different clocks*)
(*t<2 && t< 3 && t < 4 && t > 1 && t> 0 && y<2 ....*)
(*We normalize the guard to contain  a time interval (if any) for any clock*) 
(*Moreover, if the interval is empty (es t<20 && t >30) we convert it into t<0*)
(*0<t && t<2*)
(*For instance:*)
(* let l =  [(TSBClock "t", Great, 2);(TSBClock "t", Great, 3);(TSBClock "t", Less, 8); *)
(*           (TSBClock "x", Less, 10); (TSBClock "x", Less, 5); (TSBClock "x", Great, 4); *)
(*           (TSBClock "z", Less, 10)  ];;                   *)
(* normalize l;;                  *)
(* # - (tsb_clock * tsb_relation * int) list = *)
(* [(TSBClock "z", Less, 10); (TSBClock "x", Great, 4); (TSBClock "x", Less, 5); *)
(*  (TSBClock "t", Great, 3); (TSBClock "t", Less, 8)] *)


(*Get the clock list clocks from a set of clock constraints l*)
let rec getClockList l clocks = match l with 
      []-> clocks
| (TSBClock c, a, b)::tl -> getClockList tl (addElSet c clocks);; 

(*Partition the constraint list in sets belonging to the same clock*)
let rec partition clocks l = match clocks with 
  [] -> []
| hd::tl ->(hd,  List.filter (fun (TSBClock c, a, b)-> if hd = c then true else false ) l):: (partition  tl l);;

(*Creating a type for boundaries, to deal with min and max for intervals*)
type boundary = None | MinBound | MaxBound | Bound of (tsb_clock * tsb_relation * int) ;;

(*Extract the clock constraint from the bound*)
let getConstraint b = match b with 
          Bound g -> g
|  _-> failwith "Error in getConstraint" ;;

(*The tricky part with intervals is that they are the result of intersection*)
(* so that, the minimum boundary is  max point (es t>2 && t> 3 && t>4 gives t>4*)
(* and the maximum boundary is the min point*)
let getMaxBound b1 b2 = match (b1,b2) with 
    (MinBound, _) -> b2
| ( _, MinBound) -> b1
| (Bound (clock1, r1, d1), Bound(clock2,r2,d2))->     
                   if r1<>r2 then failwith "getMaxBound: not the same relation"
                   else if d1 < d2 then b2 else b1
|  _ ->  failwith "Something went wrong with getMaxBound"
;;

let getMinBound b1 b2 = match (b1,b2) with 
    (MaxBound, _) -> b2
| ( _, MaxBound) -> b1
| (Bound (clock1, r1, d1), Bound(clock2,r2,d2))->     
                   if r1<>r2 then failwith "getMinBound: not the same relation"
                   else if d1 < d2 then b1 else b2
|  _ ->  failwith "Something went wrong with getMinBound"
;;

(*Converting the partitionned list into a list of intervals: takes the list of constraint for each clock*)
(*minB is 2<t and maxB is t<10*)
let rec  makeInterval part minB maxB = match part with 
  []-> (minB, maxB)
| (c, r, d)::tl-> makeInterval tl (if r = Great then getMaxBound (Bound(c,r,d)) minB else minB)
                                  (if r = Less then getMinBound (Bound(c,r,d)) maxB else maxB)
;;  

(*Re-converting a bound list to a guardlist*)
let rec constraint_of_bound bl = match bl with
         []->[]
| hd::tl -> if hd = None || hd = MinBound || hd = MaxBound then constraint_of_bound tl 
                                              else (getConstraint(hd))::constraint_of_bound tl
;;

(*Checking if the interval is empty*)
let isEmptyInterval (minB, maxB) = match   (minB, maxB) with 
   (Bound (clock1, r1, d1), Bound(clock2,r2,d2)) -> if d1 < d2 then false else true  
|  _ -> false;;
 
(*knowing the clock name is enought*)
let getEmptyConstraint  (minB, maxB) = match   (minB, maxB) with 
   (Bound (TSBClock c, r1, d1), Bound(clock2,r2,d2)) ->   (Bound (TSBClock c, Less, 0), None) 
|  _ -> failwith "Error in getEmptyConstraint";;  

(*Normalizing a single guard: the list of constraints*)
let normalize_guard l = let cl = getClockList l [] in
                     let part = partition cl l in 
                     (*making intervals on every sublist, then checking that the interval is non empty*)
                     (*then de-coupling, then converting back to constraint. So:*)
                     (*converting back to constraints list*)
                     constraint_of_bound (
                        (*decoupling*)
                        List.fold_right (fun (a,b) y -> a::(b::y))
                           (*checking the interval is not empty*)  
                           (List.map (fun x -> if isEmptyInterval(x) then getEmptyConstraint(x)  else x)  
                              (*making interval*)
                              (List.map (fun x -> makeInterval (snd x) MinBound MaxBound) part))
                           []
                     )  ;; 


(*Normalizing a list of actions*)
let rec normalize l = match l with
   [] -> [] 
|    (a,TSBGuard g,r,o)::tl -> (a,TSBGuard(normalize_guard g), r, o)::(normalize tl);;


(****************************************************************************************************)
(*                                                                                                  *)
(*                    Build Automa                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)
(*createLabels extracts labels from tsb actions*)
let rec createLabels l   =  
   List.fold_right (fun (TSBAction a, g, r, p) y -> [(Label (bar^a));(Label a)] @ y ) l [];;

(*BuildAutoma actually builds the automa from the tsb process p*)
(*given a tsb process and the last used index, return an automa ,  a new index and a recursion list*)
(*index is a counter used to name locations in a unique way*)
(*idx it represents the last used index, so that to use it, you must increase it*)
let rec buildAutoma  p idx =  match p with 
  Success -> (successAutoma, idx, [])
|IntChoice l' -> let l = normalize l' in  
    (*Recursive step: creating automata for the suffixes*) 
    let (all_automata, used_idx, recList) = buildAutomaList (List.map getSuffix l) idx in 
    (*Creating new initial location for the internal choice*)
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in (*new initial location *)
    (*creates edges for the internal choice but non recursively: only at this level*)
    let (choice_edg, choice_idx, choice_procs, choice_clocks, choice_inv, choice_com)  = createIntEdges init l  all_automata used_idx in 
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata)) in 
    let committed  = choice_com @(sumCom_a all_automata)   in
    let inv =   ["l0_"^(string_of_int (used_idx+1)),  getMaxInv l  ] @ choice_inv @(sumInvs_a all_automata)
    (*Gather together all the sets and returs*)
    in ((TimedAutoma ("", [], init, labels, edges, inv, clocks, [],  committed , [], [], procs)), choice_idx, recList)
|ExtChoice l' ->  let l = normalize l' in  
    (*Recursive step: creating automata for the suffixes*) 
    let (all_automata, used_idx, recList) = buildAutomaList (List.map getSuffix l) idx in 
    (*Creating new initial location for the external choice*)
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in 
    (*Creates edges for the external choice but non recursively: only at this level*)
    let (choice_edg, choice_idx, choice_procs, choice_clocks,  choice_inv, choice_com)  = createExtEdges init l  all_automata used_idx in 
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata)) in
    let committed  = choice_com @(sumCom_a all_automata)    in
    let inv =  choice_inv @(sumInvs_a all_automata) 
    (*Gather together all the sets and returs*)
    in ((TimedAutoma ("", [], init, labels, edges, inv, clocks, [],  committed , [], [], procs)), choice_idx, recList )
| Rec (x,q) ->  (*Recursive step: creating automata for the suffixes*) 
                let (automa, used_idx, recList) = buildAutoma q  idx in 
                let (rec_edg, rec_recList)  = createRecEdges x (getInit(automa)) recList in 
                let edges  = rec_edg  @(getEdges  automa  )
                in (setEdges automa edges, idx, rec_recList)
| Call x -> (*Creating an empty automa with new init commited location  and passing it up*)
            let ea = emptyAutoma in 
            let init  = Loc ("l_"^(string_of_int (idx+1))) in
            let committed = ["l_"^(string_of_int (idx+1))] 
            (*Adding (x, init) as a reference for recursion, to be solved when parsing Rec x*)   
            in (setCommitted(setInit ea init) committed, idx+1, [(x,init)] )
 and 
  buildAutomaList l  idx = match l with 
      [] -> ([],idx, [])
|  hd::tl -> let (tal, usedIdx_a, recList_a) = (buildAutomaList tl idx) in 
             let (ta, usedIdx, recList) = buildAutoma hd (usedIdx_a) 
             in (ta::tal,  usedIdx, recList @ recList_a)
;;


(****Packaging the final automata:  locations  are calcolated from edges!*)
let  extractLocations edges = 
     List.fold_right (fun (Edge (src, a, b, c, tgt)) y ->  [src;tgt]@y) edges [];;


(*************************************************************************************)
(*buildAutomaMain is the main methode to convert a TSBprocess into an Uppaal automata*)
(*it needs the process p and a name/identifier for it*)
let buildAutomaMain p name= let (tap, idxp, recList)  = buildAutoma p 0 in 
    if List.length recList <> 0 then failwith "BuildAutomaMain: not all the recursive call have been solved"
    else 
      let locp = eliminateDuplicates(extractLocations(getEdges(tap))) 
      in setLocations(setName tap name)  locp 
;;

(*tsb_mapping performs the conversion of two TSBprocesses p and q into two Uppaal automata*)
(*"p" and "q" are two simple names given: it is useful only in UPPAAL*) 
let tsb_mapping p q  =  [ buildAutomaMain p "p" ; buildAutomaMain q "q"] ;;
        










