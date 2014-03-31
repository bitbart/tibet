
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
(*                    Edges                                                                         *)
(*                                                                                                  *)
(*                                                                                                  *)
(****************************************************************************************************)

(*Unfortunately UPPAAL doesnot allows for more resets on an edge, unless they are written in a procedure*)
(*createResetProc create the reset procedure and returns also the clock list*)
let createResetProc  name  l = 
   [(name,  List.fold_right (fun (CO2Clock c)  y -> c^"=0; "^y) l "" )],  
     List.map (fun (CO2Clock c)  -> Clock c) l;; (*the clock list*)

(* CreateIntEdges create the list of new edges for an internal choice.*)
(* createIntEdges: given init initial location , l a list of  [(Action "a", g, r , Success);*)
(* tadl is the list of  automata in the suffix, and an index *)
(* it returns the list of new edges, last used index new, list of procedures and list of clocks*)
let rec createIntEdges  init l  tadl idx  = match (l, tadl) with (*idx has already been used*)
   [],[] -> ([], idx, [], [],  [])
| (hd::tl, hd2::tl2) ->  let (edges_a, last_used_idx, proc_a, clockList_a, invariant_a) = createIntEdges init tl tl2 (idx) in
       let current = string_of_int (last_used_idx+1)  in
       let procName = procHD^"l1_"^current^"()" in 
       let (proc, clocksInReset) = createResetProc procName (getReset hd) in 
       let clocksInGuards = getClocksListFromGuards (getGuard hd) in 
       let clocks  = clocksInGuards @ clocksInReset @ clockList_a in 
       let edges =  [ Edge (init, Label "", co2GuardToString(getGuard hd) ,"", Loc ("l1_"^current));
            Edge (Loc ("l1_"^current), Label (bar^(getAction hd)^bang), "", procName, Loc ("l2_"^current));
            Edge (Loc ("l2_"^current), Label ((getAction hd)^query), "","", getInit hd2)] @ edges_a in
       let invariant = [] (*[("l1_"^current), "AAAAAAtizi<20"] *)
       in (edges, (last_used_idx + 1), proc @ proc_a, clocks, invariant @ invariant_a )
| _ -> failwith "Error in createIntEdges"
;;

let rec createExtEdges  init l  tadl idx  = match (l, tadl) with (*idx has already been used*)
   [],[] -> ([], idx, [], [])
| (hd::tl, hd2::tl2) ->  let (edges_a, last_used_idx, proc_a, clockList_a) = createExtEdges init tl tl2 (idx) in
       let current = string_of_int (last_used_idx+1)  in
       let procName = procHD^"l1_"^current^"()" in 
       let (proc, clockList) = createResetProc procName (getReset hd) in 
       let edges =  [ Edge (init, Label  (bar^(getAction hd)^query), "","", Loc ("l1_"^current));
            Edge (Loc ("l1_"^current), Label "", "","", Loc ("l2_"^current));
            Edge (Loc ("l2_"^current), Label ((getAction hd)^bang), "", procName, getInit hd2)] @ edges_a  
       in (edges, (last_used_idx + 1), proc @ proc_a, clockList @ clockList_a)
| _ -> failwith "Error in createExtEdges"
;;

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
let rec buildAutoma  p idx =  match p with 
  Success -> (successAutoma, idx)
|IntChoice l ->  (*let all_g = List.map getGuard l in*)
    let (all_automata, used_idx) = buildAutomaList (List.map getSuffix l) idx in 
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in (*new initial location *)
    let (choice_edg, eidx, choice_procs, choice_clocks, choice_inv)  = createIntEdges init l  all_automata used_idx in (*solo a questo livello*)
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata)) in
    let inv =   ["l0_"^(string_of_int (used_idx+1)),  getMaxInv l  ] @ choice_inv @(sumInvs_a all_automata)
    in ((TimedAutoma ("", [], init, labels, edges, inv, clocks, [],  (fun x -> false) , [], [], procs)), eidx)
|ExtChoice l ->  (*let all_g = List.map getGuard l in*)
    let (all_automata, used_idx) = buildAutomaList (List.map getSuffix l) idx in 
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in (*new initial location *)
    let (choice_edg, eidx, choice_procs, choice_clocks)  = createExtEdges init l  all_automata used_idx in (*solo a questo livello*)
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata))   
    in ((TimedAutoma ("", [], init, labels, edges, [], clocks, [],  (fun x -> false) , [], [], procs)), eidx)
 and 
  buildAutomaList l  idx = match l with 
      [] -> ([],idx)
|  hd::tl -> let (tal, usedIdx) = (buildAutomaList tl idx) in 
             let (ta, usedIdx_2) = buildAutoma hd (usedIdx) 
             in (ta::tal,  usedIdx_2)
;;


(****Packaging the final automata: clocks, locations, labels are calcolated from edges*)
let  extractLocations edges = 
     List.fold_right (fun (Edge (src, a, b, c, tgt)) y ->  [src;tgt]@y) edges [successLoc];;

(*to convert the clocks*)
let  toTAClocks l = List.map (fun  (CO2Clock c) -> Clock c) ;;

(*Uppaal automata need a name/ identifier*)
let buildAutomaMain p name= let (tap, idxp)  = buildAutoma p 0 in 
    let locp = eliminateDuplicates(extractLocations(getEdges(tap))) in
    setLocations(setName tap name)  locp 
;;

(*The function "mapping" return a network of two automata from the  processes p and q.*)
let co2_mapping p q  =  [ buildAutomaMain p "p" ; buildAutomaMain q "q"] ;;
        
