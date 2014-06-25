(*****MONITOR FUNCTIONS USAGE *********************************************************)
File monitor.ml implements montor utilities.

Functions at top levels are: 
1) m_start 
val m_start : tsb -> tsb -> tsb_network = <fun> 

Function m_start takes two TSB processes and
returns a network. A network is defined in file tipi.ml:

type tsb_network = Network of ((process_name*tsb) * (process_name*tsb) * tsb_buffer * tsb_time);;
 
contains two couples process name and TSB process, a buffer and a function which evaluates time.
Processes are given a standard name: the first is "A" and the second is "B"



2) m_step
val m_step : tsb_network -> tsb_step -> tsb_network = <fun>

Function m_step takes a network and a step and returns the network resulting from executing 
that step. Step can be of the form: delay time or fire an action:
type tsb_step = Delay of float | Fire of (process_name * performedAction);;

A performed action is of the form:

type performedAction = Int of tsb_action | Ext of tsb_action;;



3) m_onDuty
   val m_onDuty : tsb_network -> process_name * tsb = <fun>
Function m_onDuty takes a network and return name and process of the participant which is on duty.



4) m_culpable # val m_onDuty : tsb_network -> process_name * tsb = <fun>
 val m_culpable : tsb_network -> (process_name * tsb) list = <fun>
Function m_culpable a network and return a list of name and process of the participants  which are culpable.


(********************************************************************)
(*                          Example                                 *)
(********************************************************************)
let p =  IntChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;
let q =  ExtChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 1)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 2)], TSBReset[] , Success)];;

(*correct interaction*)
let net1 = m_start p q;;

let net2 = m_step net1  (Fire ("A", Int (TSBAction "a" )));;

let net3 = m_step net2  (Fire ("B", Ext (TSBAction "a" )));;


