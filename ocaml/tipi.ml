(** 
 ********************************************************************************
 **																																						 **
 **				TIPI (1): Contains abstract data types for contracts and automata    **
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
type tsb_relation = Less | Great | LessEq | GreatEq ;;
type tsb_guard = TSBGuard of (tsb_clock * tsb_relation * int) list;;
type tsb_reset = TSBReset of tsb_clock list;;

type tsb = Nil | Success |
           IntChoice of (tsb_action * tsb_guard * tsb_reset * tsb) list | 
           ExtChoice of (tsb_action *tsb_guard * tsb_reset * tsb) list  |
           Rec of string * tsb |
           Call of string ;; 


(*************************************)
(*                                   *)
(*         Timed TSB - Monitor       *)
(*                                   *)
(*************************************)
(*Environment as a function*)
(*type tsb_env = Env of ( string -> tsb);;*)
(*let emptyEnv = Env  (fun x -> Nil);;*)
(*let applyEnv (Env rho) id = rho id;;*)
(*let  bindEnv   (Env rho) id p = Env ( fun y -> if y = id then p else rho id);;*)

(*Environment as a list of couples*)
type tsb_env = Env of ( string *tsb ) list;;
let emptyEnv = Env [];;

let rec getEnv l id = match l with
  (pid, p):: tl-> if id = pid then p else getEnv tl id
| []-> Nil;;

let applyEnv (Env rho) id =  getEnv rho id;; 

let rec remove l id = match l with
  (pid, p):: tl-> if id = pid then p else getEnv tl id
| []-> Nil;;

let bindEnv (Env rho) id p = Env ((id,p)::(List.filter (fun (a,b) -> if a = id then false else true) rho));;

(*Time as a function*)
(*type tsb_time = Time of (tsb_clock-> float );;*)
(*let startTime = Time (fun x -> 0.0);;*)
(*let applyTime (Time nu) t = nu t ;;*)
(*let resetTime (Time nu) l  = Time (fun y -> if (List.mem y l) then 0.0 else nu y);;*)
(*let incrTime  (Time nu) d  = Time (fun y ->  nu y +.d);;*)

(*Time as a list of couples*)
(*Time has an absolute value then a list which contains for each clock, the time in which it  has been reset-- if needed*)
type tsb_time = Time of (float * (tsb_clock * float ) list);;
let startTime = Time (0., []);;

let rec getTime l id = match l with
  (c, d):: tl-> if id = c then d else getTime tl id
| []-> 0.;;

(*to calculate the clock time, we subtract  reset time from time*)
let applyTime (Time (curr, nu)) t = curr -. (getTime nu t) ;;

(*to reset the time for all the clock in l, we prior eliminate these clocks from nu, and then we reinsert them*)
let resetTime (Time (curr, nu)) l  =  
        Time (curr, (List.map (fun x -> (x,curr)) l)
               @(List.filter(fun (y,f) -> if (List.mem y l) then false  else true) nu ));;

let incrTime  (Time (curr, nu)) d  = Time( curr  +. d, nu);;


(*Network*)
type performedAction = Int of tsb_action | Ext of tsb_action;;

type process_name = string;;

type tsb_step = Delay of float | Fire of (process_name * performedAction);;(*process name*)

type tsb_buffer = EmptyBuffer | Buffer of process_name * tsb_action;;

type tsb_network = Network of ((process_name* tsb * tsb_env) * (process_name* tsb * tsb_env) * tsb_buffer  * tsb_time);;
let getProcessName (pid, p, env) = pid;;
let getTSBProcess (pid, p, env) = p;;
let getEnvProcess (pid, p, env) = env;;


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
let emptyAutoma = TimedAutoma ("",[],Loc "",[],[],[], [], [], [], [], [],  []);;

let idleAutoma = TimedAutoma ("",[Loc "l"],Loc "l",[],[],[], [], [], [], [], [],  []);;

(**************Getter for automa fields ********************)
let getInit  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = init;; 

let getLocations  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = locations;; 

let getLabels  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = labels;; 

let getEdges  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = edges;; 

let getInvariants  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = invariants;; 

let getProcs  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = procedures;; 

let getClocks  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = clocks;; 

let getCommitted  (TimedAutoma (name, locations, init, labels, edges, invariants, clocks, globalClocks,  committed, variables, globalVariables,  procedures)) = committed;; 

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
