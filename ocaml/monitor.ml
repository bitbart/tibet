(******************************************************************************)
(*         MAPPING (2): offers functions which map contracts into automata    *)
(******************************************************************************)
  


(*Inclusions to be used when compiling with Ocaml Interactive Environment*)
#load "str.cma";;
#use "tipi.ml";; 


(*Evaluating a guard: (a guard is a list of single constraints)*)
let evaluate' time (c,op,d) base  = 
  if op = Less then base && ( applyTime time c < (float_of_int d)) else base && ( applyTime time c > (float_of_int d));; 
let evaluate guard time = List.fold_right (evaluate' time)  guard true;;

(* Getting actions out of a list( iether internal or external choice)*)
let rec getAction l act = match l with 
  (a,b,c,d)::tl -> if (a = act) then (b,c,d) else getAction tl act
| [] ->  failwith "Action not found";;

let findAction l act = List.mem act (List.map (fun (a,b,c,d) -> a ) l);;

(*Enqueuing an action***********************************************************)
let enqueue' p b time pid act =   match (p, b) with 
  ( IntChoice l, EmptyBuffer) ->  if  not (findAction l act) then   failwith "AAA" (*(Nil, b, time)*)
                                  else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			               if  not (evaluate guard time) then  failwith ("BBB"^(toString_guard (TSBGuard guard))) (*(Nil, b, time)*) 
			               else (p', Buffer (pid,act), resetTime time reset)
			     )
  | _->  (Nil, b, time);;

let enqueue (Network ((p_id,p),(q_id,q),b,time)) pid  act =   
  if pid = p_id then let (p',b', time') =  enqueue' p b time p_id act  in  Network ((p_id,p'),(q_id,q),b',time')
  else  if pid = q_id then let (q',b', time') =  enqueue' q b time q_id act  in  Network ((p_id,p),(q_id,q'),b',time')
  else failwith "Wrong pid";;


(*Dequeuing an action***********************************************************)
let dequeue' p b time pid act =   match (p, b) with 
  ( ExtChoice l, Buffer (currBpid, currBact) ) ->  if  ( not (findAction l act) || (*act not present in choice*)
                                                       not (act = currBact) ||   (*act not  matching buffer's one*)
                                                       pid == currBpid ) (*not the right pid*) 
                                                   then  failwith "CCC" (*(Nil, b, time)*)
                            else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			    if  not (evaluate guard time) then   failwith  ("DDD"^(toString_guard (TSBGuard guard))) (*(Nil, b, time)*) else (p', EmptyBuffer, resetTime time reset)
			     )
  | _->  failwith "CCC" (*(Nil, b, time)*);;

let dequeue (Network ((p_id,p),(q_id,q),b,time)) pid  act =  
  if pid = p_id then let (p',b', time') =  dequeue' p b time p_id act  in  Network ((p_id,p'),(q_id,q),b',time')
  else  if pid = q_id then let (q',b', time') =  dequeue' q b time q_id act  in  Network ((p_id,p),(q_id,q'),b',time')
  else failwith "Wrong pid";;  




(*Delaying *********************************************************************)
let delay (Network (p,q,b,time)) d =   match b with 
  Empty -> 
    if pid = p_id then ( Network(p,q,b,time))
    else  if pid = q_id then ( Network(p,q,b,time))
    else failwith "Wrong pid"  
| _->  failwith "buffer not empty";;


(*****Top level functions*********************************************************)
(***Function "m_start" gets two TSB processes and instanciate a  network******)
let m_start p q = Network (("A",p),("B",q), EmptyBuffer, startTime);;

(***Function "m_step" gets a network,  a single step and  returns a network******)
let m_step (Network (p,q,b, time)) s = match s with 
  Delay d ->  (Network (p,q,b, time))(*delay (Network(p,q,b,time)) passo*)
| Fire (pid, Int act) ->   enqueue (Network(p,q,b,time)) pid  act 
| Fire (pid, Ext act) ->   dequeue (Network(p,q,b,time)) pid  act;;

(***Function "m_culpable" gets a network and returns a list of culpable processes******)
let m_culpable (Network (p,q,b, time)) = [p];; 

(***Function "m_onDuty" gets a network and returns the onDuty process******)
let m_onDuty (Network (p,q,b, time)) = p;; 



(********************************************************************)
(*                          Testing                                 *)
(********************************************************************)
let p =  IntChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;
let q =  ExtChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;

(*correct interaction*)
let net1 = m_start p q;;

let net2 = m_step net1  (Fire ("A", Int (TSBAction "a" )));;

let net3 = m_step net2  (Fire ("B", Ext (TSBAction "a" )));;

(*incorrect interaction*)
m_step net1  (Fire ("B", Int (TSBAction "a" )));;
m_step net1  (Fire ("B", Ext (TSBAction "a" )));;

m_step net2  (Fire ("B", Ext (TSBAction "b" )));;

m_onDuty net1;;
# - : process_name * tsb =
("A",
 IntChoice
  [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 1)], TSBReset [], Success);
   (TSBAction "b", TSBGuard [(TSBClock "t", Less, 2)], TSBReset [], Success)])
# 

;;
m_culpable net2;;
# - : process_name * tsb =
("A",
 IntChoice
  [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 1)], TSBReset [], Success);
   (TSBAction "b", TSBGuard [(TSBClock "t", Less, 2)], TSBReset [], Success)])
# 
