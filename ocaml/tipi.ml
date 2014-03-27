


(************Sets ********)
let a = 5;;
type 'a set = 'a list;;


(************Timed Event structures**********************)
type event = Event of string;;
(**Timed Enabling: (a,2),b,(c,3) means t(a) + 2 <= t(b) < t(c) + 3 *****)
type timeBoundary = TimeB of int | Infinity;;
type timeRel = None | RelEvent of string;;
type timeConstraint = TimeConstraint of (timeRel * int) * (event)* (timeRel * timeBoundary);;
type enabling = Enabling of event set * event * timeConstraint set  * int * int;;
type conflict = Conflict of event  * event ;;
type es = TimedEventStructure of event set * enabling set * conflict set;;

let getEnablings (TimedEventStructure (s1,s2,s3)) = s2;;


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


(************* Utilities to manage sets********************)
let rec eliminateDuplicates l = match l with 
[] -> []
|  hd::tl -> if List.mem hd tl then  eliminateDuplicates tl else hd::(eliminateDuplicates tl);;

let addElSet e l = if List.mem e l then l else e::l;; 

let addSetSet s1 s2 = eliminateDuplicates (s1@s2);;

let rec  enablingToString  (Enabling (es, e, tc,  a, b)) =  List.fold_right (fun (Event e) s -> e ^ s) es "";;
  
let rec flattenEnablings l = match l with 
           [] -> []
       |  (Enabling (es, e, tc , a, b)) ::tl ->  eliminateDuplicates( es@(flattenEnablings tl));;         


(************ Utilities to manage committed locations *********)
let emptyInv = fun x -> "";;

let bind f e v = fun x -> if x = e then v else f x;;

let setCommitted f e = fun x -> if x = e then true else f x;;


(*************Utilities to debug ****************************************************)
let emptyAutoma = TimedAutoma ("",[],Loc "",[],[],emptyInv, [], [], (fun x -> false)  , [], [],  []);;

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

let rec print_events (TimedEventStructure (a,b,c)) = a;;

let rec print_automataIds la = match la with
     []-> []
|  (TimedAutoma (id, locations, init, labels, edges, invariants, clocks, globalClocks,  commited, variables, globalVariables,  procedures))::tl -> id::(print_automataIds tl);; 

let printRelEvent e = match e with
   None -> "-"
| RelEvent ev -> ""^ev^"";;

(*let printTimeB t = match t with
    Infinity -> " infinity "
| TimeB n -> string_of_int n;

let printBound  (e, d) =  
         "("^(printRelEvent e)^","^(printTimeB d)^")";;*)

(*let printTC (TimeConstraint ((e1, n), Event e, (e2, d))) =  
        "("^(printRelEvent e1)^","^(string_of_int n)^"),"^e^
        ", ("^(printRelEvent e2)^","^(printTimeB d)^")"
;;*)
