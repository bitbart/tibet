
#use "tipi.ml";;
#load "str.cma";;

(*success location*)
let successLoc = Loc "f";;

(*getter functions for  [(Action "a", g, r , Success)*)
let getAction (CO2Action a,b,c,d) = a;;
let getGuard (a,b,c,d) = b;;
let getReset (a,b,CO2Reset c,d) = c;;
let getSuffix (a,b,c, d) = d;;

(*Functions to collapse together all the fields  of all the automata in the  list*)
let sumLocations_a ta_list = eliminateDuplicates(List.flatten (List.map getLocations  ta_list));;
let sumLabels_a ta_list = eliminateDuplicates(List.flatten (List.map getLabels  ta_list));;
let sumEdges_a ta_list = eliminateDuplicates(List.flatten (List.map getEdges  ta_list));;
let sumProcs_a ta_list = eliminateDuplicates(List.flatten (List.map getProcs  ta_list));;
let sumClocks_a ta_list = eliminateDuplicates(List.flatten (List.map getClocks  ta_list));;

(*Used to compone the labels*)
let bar = "bar_";
let bang = "!";
let query = "?";
let procHD = "res_"

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
   [],[] -> ([], idx, [], [])
| (hd::tl, hd2::tl2) ->  let (edges_a, last_used_idx, proc_a, clockList_a) = createIntEdges init tl tl2 (idx) in
       let current = string_of_int (last_used_idx+1)  in
       let procName = procHD^"l1_"^current^"()" in 
       let (proc, clockList) = createResetProc procName (getReset hd) in 
       let edges =  [ Edge (init, Label "", "","", Loc ("l1_"^current));
            Edge (Loc ("l1_"^current), Label (bar^(getAction hd)^bang), "",procName, Loc ("l2_"^current));
            Edge (Loc ("l2_"^current), Label ((getAction hd)^query), "","", getInit hd2)] @ edges_a  
       in (edges, (last_used_idx + 1), proc @ proc_a, clockList @ clockList_a)
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

(*createLabels extracts labels from co2 actions*)
let rec createLabels l   =  
   List.fold_right (fun (CO2Action a, g, r, p) y -> [(Label (bar^a));(Label a)] @ y ) l [];;

(*BuildAutoma actually builds the automa from the co2 process p*)
let rec buildAutoma  p idx =  match p with 
  Success -> (successAutoma, idx)
|IntChoice l ->  (*let all_g = List.map getGuard l in*)
    let (all_automata, used_idx) = buildAutomaList (List.map getSuffix l) idx in 
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in (*new initial location *)
    let (choice_edg, eidx, choice_procs, choice_clocks)  = createIntEdges init l  all_automata used_idx in (*solo a questo livello*)
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata))   
    in ((TimedAutoma ("", [], init, labels, edges, emptyInv, clocks, [],  (fun x -> false) , [], [], procs)), eidx)
|ExtChoice l ->  (*let all_g = List.map getGuard l in*)
    let (all_automata, used_idx) = buildAutomaList (List.map getSuffix l) idx in 
    let init  = Loc ("l0_"^(string_of_int (used_idx+1))) in (*new initial location *)
    let (choice_edg, eidx, choice_procs, choice_clocks)  = createExtEdges init l  all_automata used_idx in (*solo a questo livello*)
    let edges  = choice_edg @(sumEdges_a  all_automata  )  in
    let labels  = (createLabels l) @(sumLabels_a  all_automata  ) in
    let procs = choice_procs @(sumProcs_a all_automata)   in
    let clocks = eliminateDuplicates(choice_clocks @(sumClocks_a all_automata))   
    in ((TimedAutoma ("", [], init, labels, edges, emptyInv, clocks, [],  (fun x -> false) , [], [], procs)), eidx)
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
        
