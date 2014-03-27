
(************Sets ********)
type 'a set = 'a list;;

(************Timed co2*********************)
type co2_action = CO2Action of string;;
type co2_clock = CO2Clock of string;;
type co2_relation = Less | Great ;;
type co2_guard = CO2Guard of (co2_clock * co2_relation * int) list;;
type co2_reset = CO2Reset of co2_clock list;;

type co2 = Success | IntChoice of (co2_action * co2_guard * co2_reset * co2) list | ExtChoice of (co2_action *co2_guard * co2_reset * co2) list ;; 

(***Timed Automa: first field is the name -- used in UPPAAL template******************************)
(***name, locations, initial loc, labels, edges, invariants, clocks, globalClocks,  commited locations, variables, globalVariables,  procedures******************************)


type label = Label of string ;;
type loc = Loc of string;;
type guard = string;;
type reset = string;;
type edge = Edge of loc * label * guard * reset * loc;;
type invariant = string -> string;; (*le locazioni le lascio a stringhe??? o con LOC davanti*)
type clock = Clock of string;;
type automa =TimedAutoma of string * loc set * loc * label set * edge set * invariant * clock set * clock set * (loc -> bool) * string  set * string  set * (string * string) set;;

let successLoc = Loc "f";;
(**************Getter for automa fields ********************)
let getInit  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) = init;; 

let getLocations  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) = locations;; 

let getLabels  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) = labels;; 

let getEdges  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) = edges;; 

let getInvariants  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) = invariants;; 
(**************Setter for automa fields ********************)
let setName   (TimedAutoma (n, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures)) name = (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures));; 

let setClocks   (TimedAutoma (name, locations, init, labels, edges, invariants, c, globalClocks,  commited, variables, globalVariables,  procedures)) clocks = (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures));; 

(************* Utilities to manage sets********************)

let rec eliminateDuplicates l = match l with 
[] -> []
|  hd::tl -> if List.mem hd tl then  eliminateDuplicates tl else hd::(eliminateDuplicates tl);;

let addElSet e l = if List.mem e l then l else e::l;; 

let addSetSet s1 s2 = eliminateDuplicates (s1@s2);;


(************ Utilities to manage committed locations *********)
let emptyInv = fun x -> "";;

let bind f e v = fun x -> if x = e then v else f x;;

let setCommitted f e = fun x -> if x = e then true else f x;;


(*************Utilities to debug ****************************************************)
let emptyAutoma = TimedAutoma ("",[],Loc "",[],[],emptyInv, [], [], (fun x -> false)  , [], [],  []);;

let successAutoma = TimedAutoma ("",[successLoc], successLoc,[],[],emptyInv, [], [], (fun x -> false)  , [], [],  []);;

let rec print_vars la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> variables@(print_vars tl);; 

let rec print_global_vars la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> globalVariables@(print_global_vars tl);; 

let rec print_labels la = match la with
     []-> []
|   (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> labels@(print_labels tl);; 

let rec print_clocks la = match la with
     []-> []
|  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> clocks@(print_clocks tl);; 

let rec print_automataIds la = match la with
     []-> []
|  (TimedAutoma (id, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> id::(print_automataIds tl);; 


