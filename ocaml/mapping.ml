
#use "tipi.ml";;
#load "str.cma";;


(*getter functions for  [(Action "a", g, r , Success)*)
let getAction (CO2Action a,b,c,d) = a;;
let getGuard (a,CO2Guard b,c,d) = b;;
let getReset (a,b,CO2Reset c,d) = c;;
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


(*Converter for guards: from   [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)] to "t<10 && t>1"*)
let getRelation r = match r with 
  Less -> "<"
| Great -> ">";;

let getClockName (CO2Clock c) = c;;

let getClocksListFromGuards gl = List.fold_right (fun (CO2Clock c, b, d)  y -> (Clock c)::y) gl [];;

let rec co2GuardToString gl = match gl with 
    [] -> ""
| (c,r,d)::tl1::tl2-> getClockName(c)^getRelation(r)^(string_of_int d)^" && "^(co2GuardToString (tl1::tl2))
| (c,r,d)::tl1  ->   getClockName(c)^getRelation(r)^(string_of_int d)
;;



(****************************************************************************************************)
(*                                                                                                  *)
(*                    Edges and procedures for resets                                                                        *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)

(*Unfortunately UPPAAL doesnot allows for more resets on an edge, unless they are written in a procedure*)
(*createResetProc create the reset procedure and returns also the clock list*)
let createResetProc  name  l = 
   [(name,  List.fold_right (fun (CO2Clock c)  y -> c^"=0; "^y) l "" )],  
     List.map (fun (CO2Clock c)  -> Clock c) l;; (*the clock list*)

(* let createResetProc  name  l = match l with  *)
(*    [] -> ([],[]) *)
(* | (CO2Clock c)::tl -> let  (procs, clocks) =  createResetProc *)
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
       let edges =  [ Edge (init, Label "", co2GuardToString(getGuard hd) ,"", Loc ("l1_"^current));
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
            Edge (Loc ("l1_"^current), Label "", co2GuardToString(getGuard hd),"", Loc ("l2_"^current));
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
(* l is:  [ (CO2Action "a", g2, r2 , Success), (CO2Action "a", g3, r2 , Success), (CO2Action "a", g4, r2 , Success) ];;*)
(* g is :  CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;*)
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
| (CO2Clock c, r, d)::tl -> let bound =  getBoundOfASingleAction tl in 
                            if r = Great then bound else minBound bound (Until (c,d))
;;
 

let rec getBoundOfAllTheActions l b = match l with 
  [] -> b
| (a, CO2Guard gl, r, e)::tl -> let b1 = getBoundOfASingleAction gl in 
                              let m = maxBound b1 b 
                              in  getBoundOfAllTheActions tl m
;;

let getClock l = match l with 
   [] -> ""
|   (CO2Clock c, r, d)::tl -> c;;

let boundToInv b = match b with
  Forever -> ""
| Until (c,d) -> c^" < "^string_of_int d;;

(*to initialize the list with a default value for minimum I need to know the clock name*)
let getMaxInv l =  if List.length l = 0 then ""
                   else  let c = getClock (getGuard (List.nth l 0))
                   in boundToInv(getBoundOfAllTheActions l (Until (c, 1)));;


(****************************************************************************************************)
(*                                                                                                  *)
(*                    Build Automa                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)
(*createLabels extracts labels from co2 actions*)
let rec createLabels l   =  
   List.fold_right (fun (CO2Action a, g, r, p) y -> [(Label (bar^a));(Label a)] @ y ) l [];;

(*BuildAutoma actually builds the automa from the co2 process p*)
(*given a co2 process and the last used index, return an automa and a new index*)
(*index is a counter used to name locations in a unique way*)
(*idx it represents the last used index, so that to use it, you must increase it*)
let rec buildAutoma  p idx =  match p with 
  Success -> (successAutoma, idx, [])
|IntChoice l -> 
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
|ExtChoice l -> 
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

buildAutoma (IntChoice[(CO2Action "a", CO2Guard[], CO2Reset[] ,  (Call "x"))]) 1;;

(****Packaging the final automata:  locations  are calcolated from edges*)
let  extractLocations edges = 
     List.fold_right (fun (Edge (src, a, b, c, tgt)) y ->  [src;tgt]@y) edges [successLoc];;

(*to convert the clocks*)
let  toTAClocks l = List.map (fun  (CO2Clock c) -> Clock c) ;;

(*Uppaal automata need a name/ identifier*)
let buildAutomaMain p name= let (tap, idxp, recList)  = buildAutoma p 0 in 
    if List.length recList <> 0 then failwith "BuildAutomaMain: not all the recursive call have been solved"
    else
      let locp = eliminateDuplicates(extractLocations(getEdges(tap))) 
      in setLocations(setName tap name)  locp 
;;

(*The function "mapping" return a network of two automata from the  processes p and q.*)
let co2_mapping p q  =  [ buildAutomaMain p "p" ; buildAutomaMain q "q"] ;;
        
