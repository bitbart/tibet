(*****MONITOR FUNCTIONS USAGE *********************************************************)
File monitor.ml implements monitor utilities.

Functions at top levels are: 
1) m_extStart 
 val m_extStart : extTsb -> extTsb -> tsb_extNetwork = <fun>

Function m_start takes two extTsb processes and
returns a extNetwork:

type tsb_extNetwork = ExtNetwork of ((process_name* extTsb * extTsb_env) * (process_name* extTsb * extTsb_env) * tsb_buffer  * tsb_time);;
 
contains  processname, extTsb process and environment for each of the two processes, a buffer and a function which evaluates time.
Processes are given a standard name: the first is "A" and the second is "B"


2) m_extStep
 val m_extStep : tsb_extNetwork -> tsb_step -> tsb_extNetwork = <fun>

Function m_extStep takes a network and a step and returns the network resulting from executing 
that step. Steps can be of the form: delay time or fire an action:
type tsb_step = Delay of float | Fire of (process_name * performedAction);;

A performed action is of the form:

type performedAction = Int of tsb_action | Ext of tsb_action;;



3) m_onDuty
   val m_onDuty : tsb_extNetwork -> process_name * tsb = <fun>
Function m_onDuty takes a network and return a list with the names  of the participant which are on duty.



4) m_culpable  
 val m_culpable : tsb_extNetwork -> (process_name * tsb) list = <fun>
Function m_culpable a network and return a list with the  names of the participants  which are culpable.


(********************************************************************)
(*                          Example                                 *)
(********************************************************************)
let p =  ExtIntChoice[(TSBAction "a",  TSBExtGuard (SC(TSBClock "t", ExtLessEq, 1)), TSBReset[] , ExtSuccess);
                   (TSBAction "b",  TSBExtGuard (SC(TSBClock "t", ExtLess, 2)), TSBReset[] , ExtSuccess)];;
let q =  ExtExtChoice[(TSBAction "a",  TSBExtGuard (SC(TSBClock "t", ExtGreatEq, 1)), TSBReset[] , ExtSuccess);
                   (TSBAction "b",  TSBExtGuard (SC(TSBClock "t", ExtLess, 2)), TSBReset[], ExtSuccess)];;

(*correct interaction*)
let net1 = m_extStart p q;;

let check n = ("On duty:", m_onDuty n), ("Culpable:", m_culpable n);;

let net2 = m_extStep net1  (Delay 1.0 );;
check net2;;

let net3 = m_extStep net2  (Fire ("A", Int (TSBAction "a" )));;
check net3 ;;

let net4 = m_extStep net3  (Delay 6.0 );;
check net4 ;;

let net4Bis = m_extStep net3  (Fire ("B", Ext (TSBAction "a" )));;
check net4Bis ;;


let net5 = m_extStep net4Bis  (Delay 8.0 );;
check net5 ;;





