(*****************************************************)
(*                                                   *)
(*         Extended Timed TSB --extended static      *)
(*                                                   *)
(*****************************************************)
type tsb_ext_relation = Less | Great | LessEq | GreatEq | Eq;;

type extGuard =   SC of tsb_clock * tsb_ext_relation * int              (*simple constraint: es:  t < 4*) 
                | DC of tsb_clock * tsb_clock * tsb_ext_relation * int  (*diagonal constraint: es: t - x < 4*) 
                | And of extGuard * extGuard 
                | Or of extGuard * extGuard 
                | Not of extGuard;;

type tsb_extGuard =  TSBExtGuard of extGuard;;

type extTsb = Nil | Success |
           IntChoice of (tsb_action * tsb_extGuard * tsb_reset * extTsb) list | 
           ExtChoice of (tsb_action *tsb_extGuard * tsb_reset * extTsb) list  |
           Rec of string * extTsb |
           Call of string ;; 

(*Examples*)
let g = TSBExtGuard(And(Not(SC(TSBClock "x", Less, 4)),Or(Not(DC (TSBClock "x", TSBClock "t",  LessEq, 7)), DC(TSBClock "t", TSBClock "x", Eq, 5))));;
