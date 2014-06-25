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
  ( IntChoice l, EmptyBuffer) ->  if  not (findAction l act) then  (Nil, b, time)
                           else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			    if  not (evaluate guard time) then  (Nil, b, time) else (p', Buffer (pid,act), resetTime time reset)
			     )
  | _->  (Nil, b, time);;

let enqueue (Network ((p_id,p),(q_id,q),b,time)) pid  act =   
  if pid = p_id then let (p',b', time') =  enqueue' p b time p_id act  in  Network ((p_id,p'),(q_id,q),b',time')
  else  if pid = q_id then let (q',b', time') =  enqueue' q b time q_id act  in  Network ((p_id,p),(q_id,q'),b',time')
  else failwith "Wrong pid";;


(*Dequeuing an action***********************************************************)
let dequeue' p b time pid act =   match (p, b) with 
  ( ExtChoice l, Buffer (currBpid, currBact) ) ->  if  ( not (findAction l act) || (*not present in choice*)
                                                       not (act = currBact) ||   (*not the matching one*)
                                                       pid == currBpid ) (*not the right pid*) 
                                                   then  (Nil, b, time)
                            else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			    if  not (evaluate guard time) then  (Nil, b, time) else (p', Buffer (pid,act), resetTime time reset)
			     )
  | _->  (Nil, b, time);;

let dequeue (Network ((p_id,p),(q_id,q),b,time)) pid  act =  
  if pid = p_id then let (p',b', time') =  dequeue' p b time p_id act  in  Network ((p_id,p'),(q_id,q),b',time')
  else  if pid = q_id then let (q',b', time') =  dequeue' q b time q_id act  in  Network ((p_id,p),(q_id,q'),b',time')
  else failwith "Wrong pid";;  


let p =  IntChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1); (TSBClock "t", Great, 2)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2) ], TSBReset[] , Success)];;
enqueue' p  EmptyBuffer  startTime "A" (TSBAction "b") ;;
enqueue(Network (("A",p),("B",p), EmptyBuffer, startTime)) "A"  (TSBAction "b");;
enqueue(Network (("A",p),("B",p), Buffer ("A", TSBAction "a" ) , startTime)) "A"  (TSBAction "b");;

let q =  ExtChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1); (TSBClock "t", Great, 2)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2) ], TSBReset[] , Success)];;

dequeue(Network (("A",p),("B",p), EmptyBuffer, startTime)) "B"  (TSBAction "b");;
enqueue(Network (("A",p),("B",q), Buffer ("A", TSBAction "a" ) , startTime)) "B"  (TSBAction "b");;


(*Delaying *********************************************************************)
let delay (Network (p,q,b,time)) d =   match b with 
  Empty -> (
    if pid = p_id then ( Network(p,q,b,time))
    else  if pid = q_id then ( Network(p,q,b,time))
    else failwith "Wrong pid"
  )
| _->  failwith "buffer not empty";;


(*****Top level functions*********************************************************)
(***Function "m_start" gets two TSB processes and instanciate a  network******)
let m_start p q = Network (("A",p),("B",q),EmptyBuffer, startTime);;

(***Function "m_step" gets a network,  a single step and  returns a network******)
let m_step (Network (p,q,b, time)) passo = match passo with 
  Delay d ->  delay (Network(p,q,b,time)) passo
| Fire (pid, Int act) ->   enqueue (Network(p,q,b,time)) pid  act 
| Fire (pid, Ext act) ->   dequeue (Network(p,q,b,time)) pid  act;;
