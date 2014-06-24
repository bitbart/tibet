(******************************************************************************)
(*         MAPPING (2): offers functions which map contracts into automata    *)
(******************************************************************************)
  


(*Inclusions to be used when compiling with Ocaml Interactive Environment*)
#load "str.cma";;
#use "tipi.ml";; 


let delay (Network (p,q,b,time)) d =   match b with 
  Empty -> (
    if pid = p_id then ( Network(p,q,b,time))
    else  if pid = q_id then ( Network(p,q,b,time))
    else failwith "Wrong pid"
  )
| _->  failwith "buffer not empty";;


let findAction l act = List.mem act (List.map (fun (a,b,c,d) -> a ) l);;
(*da finire qui:....*)
let enqueue' p b time act =   match (b,p) with 
  (Empty, IntChoice l) ->  let curr = findAction l act in 
                           (p,b,time)
  | _->  (Nil, b, time);;

let enqueue (Network ((p_id,p),(q_id,q),b,time)) pid  act =   
  if pid = p_id then let (p',b', time') =  enqueue' p b time act  in  Network ((p_id,p'),(q_id,q),b',time')
  else  if pid = q_id then let (q',b', time') =  fireAction q b time act  in  Network ((p_id,p),(q_id,q'),b',time')
  else failwith "Wrong pid";;

let dequeue (Network (p,q,b,time)) pid  act =  match b with 
  TSBAction a-> (
    if pid = p_id then ( Network(p,q,b,time))
    else  if pid = q_id then ( Network(p,q,b,time))
    else failwith "Wrong pid"
  )
| _->  failwith "Unable to dequeue: buffer  empty";;




(*Function perform gets a network,  a single step and it returns a network*)
let m_step (Network (p,q,b, time)) passo = match passo with 
  Delay d ->  delay (Network(p,q,b,time)) passo
| Fire (pid, Int act) ->   enqueue (Network(p,q,b,time)) pid  act 
| Fire (pid, Ext act) ->   dequeue (Network(p,q,b,time)) pid  act;;
