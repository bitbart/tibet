
#use "tipi.ml";;
#load "str.cma";;


(********************************************************)
(*                                                      *)          
(*The function "buildAutoma p " returns the automa for the process p. *)
(*The construction is bottom-up.                      ****)
(*idx is used to find a new name for locations                                *)
(*                                                      *)
(********************************************************)      
(*given a choice -external or internal- getGuard return the guard element of the tuple es:  [(Action "a", g, r , Success)*)
let getAction (CO2Action a,b,c,d) = a;;
let getGuard (a,b,c,d) = b;;
let getReset (a,b,c,d) = c;;
(*given a choice -external or internal- getSuffix return the suffix element of the tuple es:  [(Action "a", g, r , Success)*)
let getSuffix (a,b,c, d) = d;;

(*Functions to collapse together all the fields  of all the automata in the  list*)
let sumLocations_a ta_list = eliminateDuplicates(List.flatten (List.map getLocations  ta_list));;
let sumLabels_a ta_list = eliminateDuplicates(List.flatten (List.map getLabels  ta_list));;
let sumEdges_a ta_list = eliminateDuplicates(List.flatten (List.map getEdges  ta_list));;

(*createLocations create the list of new locations for internal and external choice. l is a list of  [(Action "a", g, r , Success)*)
let rec createLocations l  idx = match l with (*idx has already been used*)
   [] -> ([], idx)
| hd::tl ->  let (locs, last_used_idx) = createLocations tl (idx+1) 
             in ( [ Loc ("l1_"^string_of_int (idx+1));  Loc ("l2_"^string_of_int (idx+1))] @ locs, last_used_idx) 
;;  


(*createIntEdges create the list of new edges for internal choice. l is a list of  [(Action "a", g, r , Success) tadl is the list of the automata in the suffix *)
let rec createExtEdges  init l  tadl idx = match (l, tadl) with (*idx has already been used*)
   [],[] -> []
| (hd::tl, hd2::tl2) ->  let edges = createEdges init tl tl2 (idx+1) 
       in [ (Edge (init, Label (getAction hd), "","", Loc ("l1_"^string_of_int (idx+1))));
            (Edge (Loc ("l1_"^string_of_int (idx+1)), Label (getAction hd), "","", Loc ("l2_"^string_of_int (idx+1))));
            (Edge (Loc ("l2_"^string_of_int (idx+1)), Label (getAction hd), "","", getInit hd2))] @ edges
| _ -> failwith "Error in createEdges"
;;





let rec buildAutoma  p idx =  match p with 
  Success -> successAutoma
| IntChoice l ->  let all_g = List.map getGuard l in
    let init  = Loc ("l0_"^(string_of_int idx)) in (*new initial location *)
    let (choice_locs, usedIdx) = createLocations l idx in
    let all_suffixes = List.map getSuffix l in
    let all_automata = buildAutomaList all_suffixes usedIdx in 
    let locs =   [init]@choice_locs@ (sumLocations_a  all_automata) in 
    let labels = (sumLabels_a   all_automata)  in 
    let choice_edg = createExtEdges init l  all_automata idx in
    let edges  = choice_edg @(sumEdges_a  all_automata  )  
    in (TimedAutoma ("", locs, init, labels, edges, emptyInv, [], [],  (fun x -> false) , [], [], []))
|   ExtChoice l ->  successAutoma
 and 
  buildAutomaList l  idx = match l with 
      [] -> []
|  hd::tl -> (buildAutoma hd (idx+1))::(buildAutomaList tl idx)
;;

(*to convert the clocks*)
let rec toTAClocks l = match l with 
      [] -> []
| (CO2Clock c)::tl -> (Clock c)::(toTAClocks tl);; 

(*The function "mapping" return a network of  automata from the  processes p and q given*)
let co2_mapping (p,cp) (q,cq)  =  let tap = buildAutoma p 0 in 
                      let taq = buildAutoma q 0 in 
                      [setClocks(setName tap "p") (toTAClocks cp) ; setClocks(setName taq "q") (toTAClocks cp) ];;
        (* List.map (buildAutoma  (TimedEventStructure (events, en, conflicts)))  ( getEnabledEvents en);;*)

     

