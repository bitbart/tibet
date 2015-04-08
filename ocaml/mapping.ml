(** 
 ********************************************************************************
 **                                                                            **
 **MAPPING (6): Offers functions which map contracts into automata.            **
 **																																						 **
 ********************************************************************************
 **)


(* Inclusions to be used when compiling with makefile - PLEASE IGNORE THE FOLLOWING LINE 
   open Errors;;open Tipi;;open ExtTipi;;open Python;;*)

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
(* 1) normalize all the guards  usig phython -- this is done during the tranformation into denf*)
(* 2) there are no free recursion variables*)
(* 3) there are no rec in not guarded-sequeces*)
(**)
(*The mapping maintains the following limitations:*)
(* A) guards shall not have  negations*)


(****************************************************************************************************)
(*                                                                                                  *)
(*              Disjunctive normal form and Transforming a guard  into a list of disjuncts          *)
(*                                                                                                  *)
(****************************************************************************************************)

(*get the list of disjuncts*)
let rec getDisjunctList g = match g with 
  Or (c1,c2) ->  getDisjunctList c1 @ getDisjunctList c2
|         d  -> [d]
;;


(*getDisjunctiveNormalForm of a guard*)
let getDNForm  g =  (subtract g False);;


(*negation of a guard*)
let negateGuard g = (subtract True g );;


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

choiceToDENF l count = match l with 
  []  -> failwith "Error in choiceToDENF"
| (a,b,c,d)::[] ->   let (y2, l2 , newCount2) = extTsbToDENFRec d count
                     in  ([(a, getDNFormExtGuard b, c, y2)],  l2 , newCount2)    
                    (*we return a fragment for the choice, the set of equations, and the next free id*)
| (a,b,c,d)::tl ->  let (y2, l2 , newCount2) = extTsbToDENFRec d count
                    in let ( lChoice ,  l3 , newCount3)  = choiceToDENF tl newCount2
                    in     ( (a, getDNFormExtGuard b,c,y2) :: lChoice,  l2 @ l3, newCount3 +1)
;;   

(*Converting tsb into a DENF*)
(*AND normalizing guards*)
let extTsbToDENF t = if (not(checkIdesInTsb t)) 
                      then failwith "Recurision variables  starting with XX are not allowed."
                      else let (a,b,c)  = extTsbToDENFRec t startCounter
                           in (a,b) 
                      ;;  




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

(*inv and g are guards, lab is a! or a?, r is a string representing the  reset set or a preoedure call*)
(*if urgent is true then Loc l is urgent*)
let prefixAutomaton urgent (Loc l)  (inv) (g) (Label lab) r 
                             (TimedAutoma (name, locations, init, labels, edges, invariants, 
                                               clocks, globalClocks,  committed, variables, globalVariables,  procedures)) =
              try 
                    let edg =   Edge ( Loc l, Label lab, (if g <> True then extGuardToString  g else ""), r, init) in 
                    let chans =  if (String.length lab>0) (*se la label non e' vuota*)
                                 then addElSet (Label (String.sub lab 0  (String.length lab -1 )))  labels 
                                 else  labels
                    in TimedAutoma (name, Loc l :: locations, Loc l , chans, edg :: edges, (l, extGuardToString inv) :: invariants, 
                                    clocks, globalClocks,  
																		(if urgent then l::committed else committed), variables, globalVariables, procedures)
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

(*lAut e' una lista di automi*)
let unionOfLocations  lAut  = List.fold_right (fun a b -> addSetSet( getLocations a) b ) lAut [];;
let unionOfLabels   lAut  = List.fold_right (fun a b -> addSetSet( getLabels a) b) lAut [];;
let unionOfEdges  lAut  = List.fold_right (fun a b -> getEdges a  @ b) lAut [];;
let unionOfInvariants   lAut  = List.fold_right (fun a b -> getInvariants a  @ b) lAut [];;
let unionOfClocks  lAut  = List.fold_right (fun a b -> addSetSet(getClocks a) b ) lAut [];;
let unionOfGlobalClocks  lAut  = List.fold_right (fun a b -> addSetSet(getGlobalClocks a)  b) lAut [];;
let unionOfCommitted  lAut  = List.fold_right (fun a b -> getCommitted   a @ b) lAut [];;
let unionOfVariables  lAut  = List.fold_right (fun a b -> getVariables  a @ b) lAut [];;
let unionOfGlobalVariables  lAut  = List.fold_right (fun a b -> getGlobalVariables   a @ b) lAut [];;
let unionOfProcedures  lAut  = List.fold_right (fun a b -> getProcedures  a @ b) lAut [];;

(*la guardia non serve*)
let rec ic_generateAllTheBranches  (Loc l)  lAut = match lAut with 
[] -> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, 
       clocks, globalClocks,  committed, variables, globalVariables,  procedures)):: tl 
   ->   Edge ( Loc l, Label "", "","", init) :: ic_generateAllTheBranches (Loc l) tl ;;

let internalChoiceAutomaton (Loc l) lAut  = 
            let edgs =   ic_generateAllTheBranches ( Loc l ) lAut 
            in TimedAutoma ("", Loc l :: unionOfLocations  lAut, Loc l , unionOfLabels  lAut, 
                       edgs@ unionOfEdges  lAut,  unionOfInvariants   lAut, 
                       unionOfClocks  lAut, unionOfGlobalClocks  lAut ,  l :: unionOfCommitted  lAut, 
                       unionOfVariables  lAut, unionOfGlobalVariables  lAut, unionOfProcedures  lAut);;


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
let externalChoiceAutomaton (Loc l)   lAut  = 
    try
       let edgs =   ec_generateAllTheBranches ( Loc l ) lAut in 
       (*eliminating query char*) 
       let labs = List.map (fun (Label x) -> Label (if String.length x > 0 
                                                    then String.sub x 0  (String.length x -1 ) 
                                                    else "")) (getLabelsFromEdges edgs) in  
        let lAut2  = List.map (fun (a,b,c,d) -> d)  lAut 
        in TimedAutoma ("", Loc l :: unionOfLocations  lAut2, Loc l , addSetSet labs (unionOfLabels  lAut2), 
                   edgs@ unionOfEdges  lAut2, unionOfInvariants  lAut2, 
                   unionOfClocks  lAut2, unionOfGlobalClocks lAut2,  unionOfCommitted lAut2, 
                   unionOfVariables  lAut2, unionOfGlobalVariables lAut2, unionOfProcedures lAut2)
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

let sumAutomata (Loc l)  lAut =  
         TimedAutoma ("", sumLocations_a lAut, Loc l , sumLabels_a lAut, 
                          sumEdges_a lAut, sumInvs_a lAut, 
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

(*  returns a couple: if reset set is more than 1 we have (procedure name , (proc Name + procedure body))*)
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

(*Get the clock list clocks from a set of clock constraints l*)
let rec getClockListFromGuard  g  = match g with 
  True -> []
| False -> []
| SC (TSBClock t, rel, d) ->  [t]
| And (g1,g2) ->  addSetSet (getClockListFromGuard g1 ) (getClockListFromGuard g2) 
| DC (TSBClock t1, TSBClock t2, rel, d) ->  [t1; t2] 
| _ -> failwith "getClockListFromGuard: to be implemented" 
;;

(*restituisce piu' automi, uno per ogni disgiunto della lista lg,*)
(* e la prima locazione ha invariante che e' il past della guardia*)


let rec  manageInternalBranch  src  a  lg r rv count   = match lg with 
  [] -> []
| hd::tl -> 
       let newClocks = addSetSet ( List.map  (fun c  -> Clock c) (getClockListFromGuard hd)) (getClocksList r) in
       let (procName,b) =  manageResetSet r (src^a^rv^(string_of_int count)) in 
			 let labLoc =  (src^a^rv^(string_of_int count)) in
		   let aut = prefixAutomaton false ( Loc labLoc) (past hd) (hd) (Label (""))  ""  
			              (prefixAutomaton true  ( Loc (labLoc^"_bis")) (True) (hd) (Label ( a^bang))  procName (idleAutomaton (Loc rv)) )
       in  (addClocks  (addProcedure aut b) newClocks) :: 
                          (manageInternalBranch src a tl r rv (count+1))
;;

(*rv is the recursion variable of the following of the defining equation*)
(*maps the branches of an internal choice, and manage all the dsjuncts of the guards*)
(*1 is the counter...*)
(*src is the neame of the previous node*)


let rec mapInternalChoiceContinuation src l   =   match l with 
[] ->  []
| (TSBAction a, TSBExtGuard g, TSBReset r, rv):: tl ->     
       let count = 1 in
       let autList = manageInternalBranch  src a (getDisjunctList g)  r rv count
       in  autList @ mapInternalChoiceContinuation src tl
;;


let rec manageExternalBranch src a  lg r  rv  count = match lg with 
  [] -> []
| hd::tl -> 
       let newClocks = addSetSet ( List.map  (fun c  -> Clock c) (getClockListFromGuard hd)) (getClocksList r) in
       let (n,b) =  manageResetSet r (src^a^rv^(string_of_int count))  in 
       let aut =  (idleAutomaton (Loc rv))
       in  (TSBAction a, TSBExtGuard  hd, n, (addClocks  (addProcedure aut b) newClocks))
               :: (manageExternalBranch src a tl r rv count)
;;

let rec mapExternalChoiceContinuation src l = match l with 
[] -> []
| (TSBAction a, TSBExtGuard g, TSBReset r, rv):: tl ->   
	     let count = 1 in
       let  autList = manageExternalBranch  src a  (getDisjunctList g) r  rv count
       in   autList @ mapExternalChoiceContinuation src tl
;;



let denfToUppaal (x,p) = match p with   
   DESuccess ->  successAutomaton (Loc x) 
|  DEIntChoice  l -> let lAut = mapInternalChoiceContinuation x l in
                     internalChoiceAutomaton (Loc (x)) lAut 
|  DEExtChoice  l -> let lAut = mapExternalChoiceContinuation  x l in
                     externalChoiceAutomaton (Loc x)   lAut 
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