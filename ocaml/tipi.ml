(** 
 ********************************************************************************
 **																																						 **
 **				TIPI (2): Contains abstract data types for contracts and automata.   **
 **																																						 **
 ********************************************************************************
 **)

(*************************************)
(*                                   *)
(*               Sets                *)
(*                                   *)
(*************************************)
type 'a set = 'a list;;

let rec eliminateDuplicates l = match l with 
   [] -> []
|  hd::tl -> if List.mem hd tl then  eliminateDuplicates tl 
                               else hd::(eliminateDuplicates tl);;

let addElSet e l = if List.mem e l then l else e::l;; 

let addSetSet s1 s2 = eliminateDuplicates (s1@s2);;


(*************************************)
(*                                   *)
(*      Timed TSB -- Static          *)
(*                                   *)
(*************************************)
type tsb_action = TSBAction of string;;
type tsb_clock = TSBClock of string;;
type tsb_relation = Less | Great | LessEq | GreatEq | Eq ;;
type tsb_guard = TSBGuard of (tsb_clock * tsb_relation * int) list;;
type tsb_reset = TSBReset of tsb_clock list;;

type tsb = Nil | Success |
           IntChoice of (tsb_action * tsb_guard * tsb_reset * tsb) list | 
           ExtChoice of (tsb_action *tsb_guard * tsb_reset * tsb) list  |
           Rec of string * tsb |
           Call of string ;; 



(**********************************************************)
(*                                                        *)
(*         Timed Automata                                 *)
(*                                                        *)
(* (as  in UPPAAL template)                               *)
(* Fields: name, locations, initial loc, labels, edges,   *) 
(*         invariants, clocks, globalClocks,              *)
(*         committed locations,                           *)
(*         variables, globalVariables,  procedures        *)
(*                                                        *)
(**********************************************************)
type label = Label of string ;;
type loc = Loc of string;;
type guard = string;;
type reset = string;;
type edge = Edge of loc * label * guard * reset * loc;;
type invariant = (string  * string) list;;
type clock = Clock of string;;
type automa =TimedAutoma of 
                string *      (*process name*)
                loc set *     (*location list*)
                loc *         (* initial location*)
                label set *   (*label list*)
                edge set *    (*edge list*)
                invariant *   (*invariant list*)
                clock set *   (*private clock list*)
                clock set *   (*global clock list*)
                string set *  (*committed location list*)
                string  set * (*private variable list*)
                string  set * (*global variable list*)
                (string * string) set;; (*procedure list*)

(**************Constructors ********************)
let emptyAutomaton = TimedAutoma ("",[],Loc "",[],[],[], [], [], [], [], [],  []);;



(**************Getter for automa fields ********************)
let getInit  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = init;; 

let getLocations  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = locations;; 

let getLabels  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = labels;; 

let getEdges  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = edges;; 

let getInvariants  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = invariants;; 

let getProcs  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = procedures;; 

let getClocks  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = clocks;; 

let getGlobalClocks  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = globalClocks;; 

let getCommitted  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = committed;; 

let getVariables  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = variables;; 

let getGlobalVariables  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = globalVariables;; 

let getProcedures  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = procedures;; 
(**************Setter for automa fields ********************)
let setName   (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) name' = (TimedAutoma (name', locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures));; 

let setClocks   (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) clocks' = (TimedAutoma (name, locations, init, labels, edges, invariants, clocks', globalClocks,  committed, variables, globalVariables,  procedures));; 

let setLocations   (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) locs' = (TimedAutoma (name, locs', init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures));; 

let setLabels   (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) labels' = (TimedAutoma (name, locs, init, labels', edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures));; 

let setInit   (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) init' = (TimedAutoma (name, locs, init', labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures));;

let setCommitted   (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) committed' = (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed', variables, globalVariables,  procedures));;

let setEdges   (TimedAutoma (name, locs, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) edges' = (TimedAutoma (name, locs, init, labels, edges', invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures));;
(********************************************)
(*                                          *)
(*    Debugging utilities                   *)
(*                                          *)
(********************************************)
let rec print_vars la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures))::tl -> variables@(print_vars tl);; 

let rec print_global_vars la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures))::tl -> globalVariables@(print_global_vars tl);; 

let rec print_labels la = match la with
     []-> []
|   (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures))::tl -> labels@(print_labels tl);; 

let rec print_clocks la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures))::tl -> clocks@(print_clocks tl);; 

let rec print_automataIds la = match la with
     []-> []
|  (TimedAutoma (id, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures))::tl -> id::(print_automataIds tl);; 



(*********Debugging utilities*********)
let rec toString_guard (TSBGuard la)  = match la with
     []-> ""
|  (c,r,d)::tl ->  (string_of_int d)^toString_guard (TSBGuard tl);; 
