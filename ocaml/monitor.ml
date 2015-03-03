(** 
 ********************************************************************************
 **                                                                            **
 **             MONITOR (10): Contains monitoring functions and types          **
 **                                                                            **
 ********************************************************************************
 **)


(* Inclusions to be used when compiling with makefile - PLEASE IGNORE THE FOLLOWING LINE
   open Errors;;open Tipi;;open ExtTipi;;open Mapping;;open ToXML;;open FromXML;;open Python;;open Kindsystem;; *)


(*************************************)
(*                                   *)
(*Types for monitor                  *)
(*                                   *)
(*************************************)
(*Environment as a function*)
(*type tsb_env = Env of ( string -> tsb);;*)
(*let emptyEnv = Env  (fun x -> Nil);;*)
(*let applyEnv (Env rho) id = rho id;;*)
(*let  bindEnv   (Env rho) id p = Env ( fun y -> if y = id then p else rho id);;*)

(*Environment as a list of couples*)
type extTsb_env = Env of ( string * extTsb ) list;;
let emptyEnv = Env [];;

let rec getEnv l id = match l with
  (pid, p):: tl-> if id = pid then p else getEnv tl id
| []-> ExtNil;;

let applyEnv (Env rho) id =  getEnv rho id;; 

let rec remove l id = match l with
  (pid, p):: tl-> if id = pid then p else getEnv tl id
| []-> ExtNil;;

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

type tsb_extNetwork = ExtNetwork of ((process_name* extTsb * extTsb_env) * (process_name* extTsb * extTsb_env) * tsb_buffer  * tsb_time);;

let getProcessName (pid, p, env) = pid;;
let getTSBProcess (pid, p, env) = p;;
let getEnvProcess (pid, p, env) = env;;



(***************************************************************************)
(*                             Guard evaluation                            *)
(***************************************************************************)

(*conversion of tsb_ext_relation in the corresponding ocaml function*)
let op_of_rel rel = match rel with
    ExtLess -> (<)
  | ExtGreat -> (>)
  | ExtLessEq -> (<=)
  | ExtGreatEq -> (>=)
  | ExtEq -> (==);;

(*checks if a clock evaluation nu satisfy the guard g. nu must be of type: tsb_clock -> float*)
let rec evaluate g time  = match g with
    False -> false
  | True -> true
  | SC(x,rel,d) -> (op_of_rel rel) (applyTime time x) (float_of_int d)
  | DC(x,y,rel,d) -> (op_of_rel rel) ((applyTime time x) -. (applyTime time y)) (float_of_int d)
  | And(g,g') -> (evaluate g time ) && (evaluate  g' time)
  | Or(g,g') -> (evaluate  g time ) || (evaluate  g' time)
  | Not(g) -> not (evaluate  g time);;


(***************************************************************************)
(*                                                                         *)
(***************************************************************************)

(*given a process, returns its name*)
let getProcessName (pid, p, env) = pid;;

(* Getting actions out of a list( either internal or external choice)*)
let rec getAction l act = match l with 
  (a,b,c,d)::tl -> if (a = act) then (b,c,d) else getAction tl act
| [] ->  failwith _ERR_100;;

let findAction l act = List.mem act (List.map (fun (a,b,c,d) -> a ) l);;


(*Enqueuing an action p is an action , b is the buffer *************************************************)
let rec enqueue' p b rho  time pid act =   match (p, b) with 
  ( ExtIntChoice l, EmptyBuffer) ->  if  not (findAction l act) then   (ExtNil, b, rho,  time)
                                     else let (TSBExtGuard guard, TSBReset reset, p') = getAction l act 
                                          in (
			                      if  not (evaluate  guard time ) then  (ExtNil, b, rho,  time) 
			                      else (p', Buffer (pid,act), rho, resetTime time reset)
			                  ) 
  | (ExtRec (id, p'),_) ->   enqueue' p' b (bindEnv rho id p') time pid act
  | (ExtCall id, _) -> enqueue' (applyEnv rho id) b rho time pid act
  | _->  (ExtNil, b, rho, time);;



let enqueue (ExtNetwork ((p_id,p,rhop),(q_id,q,rhoq),b, time)) pid  act =   
  if pid = p_id then let (p',b', rho', time') =  enqueue' p b rhop time p_id act  in  ExtNetwork ((p_id,p',rho'),(q_id,q,rhoq),  b', time')
  else  if pid = q_id then let (q',b', rho', time') =  enqueue' q b rhoq time q_id act  in  ExtNetwork ((p_id,p,rhop),(q_id,q',rho'), b', time')
  else failwith "Wrong pid";;


(*Dequeuing an action***********************************************************)
let rec dequeue' p b rho time pid act =   match (p, b) with 
  ( ExtExtChoice l, Buffer (currBpid, currBact) ) ->  if  ( not (findAction l act) || (*act not present in choice*)
                                                       not (act = currBact) ||   (*act not  matching buffer's one*)
                                                       pid == currBpid ) (*not the right pid*) 
                                                   then  (ExtNil, b, rho, time)
                            else let (TSBExtGuard guard, TSBReset reset, p') = getAction l act 
                                 in (
			              if  not (evaluate guard time) then  (ExtNil, b, rho, time) 
                                      else (p', EmptyBuffer, rho, resetTime time reset)
			         )
  | (ExtRec (id, p'),_) ->   dequeue' p' b (bindEnv rho id p') time pid act
  | (ExtCall id, _) -> dequeue' (applyEnv rho id) b rho time pid act
  | _->  (ExtNil, b, rho, time);;

let dequeue (ExtNetwork ((p_id,p,rhop),(q_id,q,rhoq),b, time)) pid  act =  
  if pid = p_id then let (p',b', rho', time') =  dequeue' p b rhop time p_id act  in  ExtNetwork ((p_id,p',rho'),(q_id,q,rhoq),b',time')
  else  if pid = q_id then let (q',b', rho', time') =  dequeue' q b rhoq time q_id act  in  ExtNetwork ((p_id,p, rhop),(q_id,q',rho'),b',time')
  else failwith _ERR_101;;  


(*delaying: if the buffer contains something, then the other participant is gone Nil*)
let delay  (ExtNetwork (p,q,b, time))  d = match b with 
    Buffer (currBpid, currBact) -> if  getProcessName p = currBpid 
                                       then (ExtNetwork (p,  ((getProcessName q), ExtNil, (getEnvProcess q)) ,b, incrTime time d )) 
                                       else (ExtNetwork ( ((getProcessName p), ExtNil, (getEnvProcess p)),q,b, incrTime time d )) 
| EmptyBuffer -> (ExtNetwork (p,q,b, incrTime time d ));;


(***************************************************************************)
(*                   Main methodes                                         *)
(*                                                                         *)
(***************************************************************************)


(* p and q are  extTsb*)
let m_extStart p q = 
      ExtNetwork (("A",p,emptyEnv),("B",q,emptyEnv), EmptyBuffer,  startTime);;

let m_extStep (ExtNetwork (p,q,b, time)) s = match s with 
  Delay d ->  delay (ExtNetwork(p,q,b, time)) d
| Fire (pid, Int act) ->   enqueue (ExtNetwork(p,q,b, time)) pid  act 
| Fire (pid, Ext act) ->   dequeue (ExtNetwork(p,q,b, time)) pid  act;;

(*************************************************************************************************)
(*Culpability and onDuty*)
(*************************************************************************************************)
(*Managing recursion*)
let rec unfold (id, p, rho) = match p with 
  ExtRec (id, p') -> unfold  (id, p', rho)
| ExtCall id -> let p' =  applyEnv rho id in if p' == ExtNil then  failwith (_ERR_102^id) else p'
| _ -> p ;;

let rec actionIsPossible l time = match l with 
  [] -> false
| (a,TSBExtGuard g,r,p)::tl ->  (evaluate g time) || (actionIsPossible tl time);;
 
let isCulpable p  time = match (unfold  p) with 
  ExtNil -> true
| ExtIntChoice l -> if not (actionIsPossible l time) then true else false 
| _ -> false;;

let m_culpable (ExtNetwork (p, q, b, time)) =  
                 (if isCulpable p  time then [getProcessName p] else []) @  
                 (if isCulpable  q   time then [getProcessName q] else []);; 

let getId (a,b,c) = a;;

let m_onDuty (ExtNetwork (p,q,b,  time)) = match ( (unfold p), (unfold q),b) with 
    ( p', q', Buffer(idb, actb)) -> if not (isCulpable p  time ) &&  (getId p) != idb 
                                    then [getId p]
                                    else if not (isCulpable q  time ) &&  (getId q) != idb 
                                         then [getId q] else []
|  ( ExtIntChoice lp,  ExtIntChoice lq, EmptyBuffer)-> 
               (if not (isCulpable q  time ) then [getId q] else []) @ 
               (if not (isCulpable p   time ) then [getId p] else []) 
|  ( ExtIntChoice l,  q', EmptyBuffer)-> if not (isCulpable p   time ) then [getId p] else []
|  ( p',  ExtIntChoice l, EmptyBuffer)-> if not (isCulpable q   time ) then [getId q] else [] 
|  _ ->  []  ;;

 
let getProcessById p q id =  if getId p == id then p else q;;

let getMoves p time =  match  (unfold p) with  
   ExtIntChoice l ->  List.map (fun  (a,TSBExtGuard g,r,p) -> (Int a,TSBExtGuard g)) l
|  ExtExtChoice l ->  List.map (fun  (a,TSBExtGuard g,r,p) -> (Ext a,TSBExtGuard g)) l
|  _ ->  []  ;;


let m_possibleActions (ExtNetwork (p,q,b,  time)) =  let l = m_onDuty (ExtNetwork (p,q,b,  time)) in 
                           if List.length l = 0 then []
                           else( if List.length l = 1 then 
                                 let id = List.nth  l 0 
                                 in [ (id, getMoves (getProcessById p q id) time)]
                                 else   
                                       let id1 = List.nth  l 0 in
                                       let id2 = List.nth  l 1 
                                       in [ (id1, getMoves (getProcessById p q id1) time);
                                            (id2, getMoves (getProcessById p q id2) time) ]
                           )
                       ;;




(*************************************************)
(*                SERIALIZATION                  *)
(*************************************************)
let serialize_net n f =
	let chan1 = open_out_bin f in
	output_value chan1 n; flush chan1
;;

let deserialize_net f =
	let chan1 = open_in_bin f in
	let net = input_value chan1 in
	net
;;

let rec findDualTag attrs =
  match attrs with
  | ("dual", "1")::l' -> true
  | a::l' -> findDualTag l'
  | [] -> false
;;

let is_dual xml =
	let input = Xml.parse_string xml in
  match input with
  | Xml.Element ("contract", attrs, c) -> findDualTag attrs
  | _ -> failwith _ERR_103
;;

let tmp_readXmlContract p = 
	if (is_dual p) then dualof (toExtTsb (readXmlContract p)) 
	else toExtTsb (readXmlContract p)
;;

let start_mon p q filename = 
	let net1 = m_extStart (tmp_readXmlContract p) (tmp_readXmlContract q) in
	serialize_net net1 filename
;;

let fire_act pn act d fn fn' check =
	let net1 = deserialize_net fn in
	let proc = if (pn == 0) then "A" else "B" in
	let proc' = if (pn == 0) then "B" else "A" in
	let net2 = m_extStep net1 (Delay d) in
	let net3 = m_extStep net2 (Fire (proc, Int (TSBAction act))) in
	let net4 = m_extStep net3 (Fire (proc', Ext (TSBAction act))) in
	if (check == 0) then serialize_net net4 fn' else serialize_net net3 fn'
;;

let rec isCulpab' p l =
	match l with
	| a::l' -> if (String.compare a p == 0) then true else isCulpab' p l'
	| [] -> false
;;

let isCulpab p fn =
	let proc = if (p==0) then "A" else "B" in
	let net = deserialize_net fn in
	if (isCulpab' proc (m_culpable net)) then "yes" else "no"
;;

let rec isOnDuty' p l =
    match l with
    | a::l' -> if (String.compare a p == 0) then true else isOnDuty' p l'
    | [] -> false
;;

let isOnDuty p fn =
    let proc = if (p==0) then "A" else "B" in
    let net = deserialize_net fn in
    if (isOnDuty' proc (m_onDuty net)) then "yes" else "no"
;;

let isEnded fn = (String.compare (isOnDuty 0 fn) "no" == 0) && (String.compare (isOnDuty 1 fn) "no" == 0) 
								&& (String.compare (isCulpab 0 fn) "no" == 0) && (String.compare (isCulpab 1 fn) "no" == 0);;

let delay_net d fn fn' =
    let net1 = deserialize_net fn in
    let net2 = m_extStep net1 (Delay d) in
		serialize_net net2 fn'
;;

let rec extractActions b = 
	match b with
	| (Int (TSBAction a), c)::b' -> a :: (extractActions b')
	| [] -> []
	| _ -> failwith "Invalid element found in extractActions"
;;

let rec listOfActionNames l r = 
	match l with
	| (a, b)::l' -> if ((r == 0 && String.compare "A" a == 0) || (r == 1 && String.compare "B" a == 0)) then (extractActions b) @ (listOfActionNames l' r) else (listOfActionNames l' r) 
	| [] -> []
;;

let rec formatActions' l =
	match l with
	| a::l' -> "\t<action name=\"" ^ a ^ "\" time=\"-1\" />\n" ^ formatActions' l'
	| [] -> "</actionlist>"
;; 

let formatActions l = "<actionlist>\n" ^ formatActions' l;; 

let get_actions fn r =
    let net = deserialize_net fn in
    formatActions (listOfActionNames (m_possibleActions net) r)
;;