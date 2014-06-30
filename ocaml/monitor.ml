(******************************************************************************)
(*         MAPPING (2): offers functions which map contracts into automata    *)
(******************************************************************************)
  
(*Inclusions to be used when compiling with Ocaml Interactive Environment*)
#load "str.cma";;
#use "tipi.ml";; 

(*given a process, returns its name*)
let getProcessName (pid, p, env) = pid;;

(*Evaluating a single guard: (a guard is a list of single constraints)*)
let evaluate' time (c,op,d) base  = 
  if op = Less then base && ( applyTime time c < (float_of_int d)) else base && ( applyTime time c > (float_of_int d));; 
let evaluate guard time = List.fold_right (evaluate' time)  guard true;;

(* Getting actions out of a list( iether internal or external choice)*)
let rec getAction l act = match l with 
  (a,b,c,d)::tl -> if (a = act) then (b,c,d) else getAction tl act
| [] ->  failwith "Action not found";;

let findAction l act = List.mem act (List.map (fun (a,b,c,d) -> a ) l);;


(*Enqueuing an action***********************************************************)
let rec enqueue' p b rho  time pid act =   match (p, b) with 
  ( IntChoice l, EmptyBuffer) ->  if  not (findAction l act) then   (Nil, b, rho,  time)
                                  else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			               if  not (evaluate guard time) then  (Nil, b, rho,  time) 
			               else (p', Buffer (pid,act), rho, resetTime time reset)
			     ) 
  | (Rec (id, p'),_) ->   enqueue' p' b (bindEnv rho id p') time pid act
  | (Call id, _) -> enqueue' (applyEnv rho id) b rho time pid act
  | _->  (Nil, b, rho, time);;



let enqueue (Network ((p_id,p,rhop),(q_id,q,rhoq),b, time)) pid  act =   
  if pid = p_id then let (p',b', rho', time') =  enqueue' p b rhop time p_id act  in  Network ((p_id,p',rho'),(q_id,q,rhoq),  b', time')
  else  if pid = q_id then let (q',b', rho', time') =  enqueue' q b rhoq time q_id act  in  Network ((p_id,p,rhop),(q_id,q',rho'), b', time')
  else failwith "Wrong pid";;


(*Dequeuing an action***********************************************************)
let rec dequeue' p b rho time pid act =   match (p, b) with 
  ( ExtChoice l, Buffer (currBpid, currBact) ) ->  if  ( not (findAction l act) || (*act not present in choice*)
                                                       not (act = currBact) ||   (*act not  matching buffer's one*)
                                                       pid == currBpid ) (*not the right pid*) 
                                                   then  (Nil, b, rho, time)
                            else let (TSBGuard guard, TSBReset reset, p') = getAction l act in (
			    if  not (evaluate guard time) then  (Nil, b, rho, time) else (p', EmptyBuffer, rho, resetTime time reset)
			     )
  | (Rec (id, p'),_) ->   dequeue' p' b (bindEnv rho id p') time pid act
  | (Call id, _) -> dequeue' (applyEnv rho id) b rho time pid act
  | _->  (Nil, b, rho, time);;

let dequeue (Network ((p_id,p,rhop),(q_id,q,rhoq),b, time)) pid  act =  
  if pid = p_id then let (p',b', rho', time') =  dequeue' p b rhop time p_id act  in  Network ((p_id,p',rho'),(q_id,q,rhoq),b',time')
  else  if pid = q_id then let (q',b', rho', time') =  dequeue' q b rhoq time q_id act  in  Network ((p_id,p, rhop),(q_id,q',rho'),b',time')
  else failwith "Wrong pid";;  

(*delaying: if the buffer contains something, then the other participant is gone Nil*)


let delay  (Network (p,q,b, time))  d = match b with 
    Buffer (currBpid, currBact) -> if  getProcessName p = currBpid then (Network (p,  ((getProcessName q), Nil, (getEnvProcess q)) ,b, incrTime time d )) 
                                                                   else (Network ( ((getProcessName p), Nil, (getEnvProcess p)),q,b, incrTime time d )) 
| EmptyBuffer -> (Network (p,q,b, incrTime time d ));;

let m_start p q = 
       Network (("A",p,emptyEnv),("B",q,emptyEnv), EmptyBuffer,  startTime);;

let m_step (Network (p,q,b, time)) s = match s with 
  Delay d ->  delay (Network (p,q,b, time)) d
| Fire (pid, Int act) ->   enqueue (Network(p,q,b, time)) pid  act 
| Fire (pid, Ext act) ->   dequeue (Network(p,q,b, time)) pid  act;;



(*************************************************************************************************)
(*Culpability and onDuty*)
(*************************************************************************************************)
(*Managing recursion*)
let rec unfold (id, p, rho) = match p with 
  Rec (id, p') -> unfold  (id, p', rho)
| Call id -> let p' =  applyEnv rho id in if p' == Nil then  failwith ("Unbound id: Call"^id) else p'
| _ -> p ;;

let rec actionIsPossible l time = match l with 
  [] -> false
| (a,TSBGuard g,r,p)::tl ->  (evaluate g time) || (actionIsPossible tl time);;
 
let isCulpable p  time = match (unfold  p) with 
  Nil -> true
| IntChoice l -> if not (actionIsPossible l time) then true else false 
| _ -> false;;

let m_culpable (Network (p, q, b, time)) =  (if isCulpable p  time then [getProcessName p] else []) @  (if isCulpable  q   time then [getProcessName q] else []);; 

let getId (a,b,c) = a;;
let m_onDuty (Network (p,q,b,  time)) = match ( (unfold p), (unfold q),b) with 
    ( p', q', Buffer(idb, actb)) -> if not (isCulpable p  time ) &&  (getId p) != idb then [getId p]
                                    else if not (isCulpable q  time ) &&  (getId q) != idb then [getId q] else []
|  ( IntChoice lp,  IntChoice lq, EmptyBuffer)-> 
               (if not (isCulpable q  time ) then [getId q] else []) @ (if not (isCulpable p   time ) then [getId p] else []) 
|  ( IntChoice l,  q', EmptyBuffer)-> if not (isCulpable p   time ) then [getId p] else []
|  ( p',  IntChoice l, EmptyBuffer)-> if not (isCulpable q   time ) then [getId q] else [] 
|  _ ->  []  ;;



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
m_culpable net2;;
m_onDuty net2;;
let net3 = m_step net2  (Fire ("B", Ext (TSBAction "a" )));;
m_culpable net3;;
m_onDuty net3;;
let net4 = m_step net3  (Fire ("A", Ext (TSBAction "a" )));;
(*incorrect interaction*)
let c1 = m_step net1  (Fire ("B", Int (TSBAction "a" )));;
m_culpable c1;;

let c2 = m_step net1  (Fire ("B", Ext (TSBAction "a" )));;
m_culpable c2;;

let c3 = m_step net2  (Fire ("B", Ext (TSBAction "b" )));;
m_culpable c3;;

(********************************************************************)
(*                 Testing     Recursion                            *)
(********************************************************************)
let p = Rec ("x", IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x");
                             (TSBAction "b", TSBGuard[], TSBReset[] , Success)]);;
let q = Rec ("x", ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x");
                             (TSBAction "b", TSBGuard[], TSBReset[] , Success)]);;


let net1 = m_start p q;;

let net2 = m_step net1  (Fire ("A", Int (TSBAction "a" )));;
m_culpable net2;;
m_onDuty net2;;

let net3 = m_step net2  (Fire ("B", Ext (TSBAction "a" )));;
m_culpable net3;;
m_onDuty net3;;

let net4 = m_step net3  (Fire ("A", Int (TSBAction "a" )));;
m_culpable net4;;
m_onDuty net4;;

let net5 = m_step net4  (Fire ("B", Ext (TSBAction "a" )));;
m_onDuty net5;;

let net6 = m_step net5  (Fire ("A", Int (TSBAction "b" )));;
m_onDuty net6;;

let net7 = m_step net6  (Fire ("B", Ext (TSBAction "b" )));;
m_onDuty net7;;

(********************************************************************)
(*                 Testing     Time                            *)
(********************************************************************)
let p =  IntChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;
let q =  ExtChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;

(*correct interaction*)
let net1 = m_start p q;;

let net2 = m_step net1  (Fire ("A", Int (TSBAction "a" )));;
let net3 = m_step net2  (Delay 4.);;
m_culpable net3;;
m_onDuty net2;;
let net3 = m_step net2  (Fire ("B", Ext (TSBAction "a" )));;
m_culpable net3;;
m_onDuty net3;;
let net4 = m_step net3  (Fire ("A", Ext (TSBAction "a" )));;

let t0 = startTime;;
let t1 = incrTime t0 4.;;
applyTime t1 (TSBClock "x");;
let t2 = resetTime t1 [TSBClock "x";TSBClock "y" ];;
let t3 = incrTime t2 1.;;
applyTime t3 (TSBClock "x");;
