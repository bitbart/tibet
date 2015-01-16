(*****************************************************)
(*                                                   *)
(*         Extended Timed TSB --extended static      *)
(*                                                   *)
(*****************************************************)

type tsb_ext_relation = ExtLess | ExtGreat | ExtLessEq | ExtGreatEq | ExtEq;;

type extGuard =   SC of tsb_clock * tsb_ext_relation * int              (*simple constraint: es:  t < 4*) 
                | DC of tsb_clock * tsb_clock * tsb_ext_relation * int  (*diagonal constraint: es: t - x < 4*) 
                | And of extGuard * extGuard 
                | Or of extGuard * extGuard 
                | Not of extGuard
		| True
		| False;;

type tsb_extGuard =  TSBExtGuard of extGuard;;
type tsb_action = TSBAction of string;;
type tsb_reset = TSBReset of tsb_clock list;;

type extTsb = ExtNil | ExtSuccess |
           ExtIntChoice of (tsb_action * tsb_extGuard * tsb_reset * extTsb) list | 
           ExtExtChoice of (tsb_action *tsb_extGuard * tsb_reset * extTsb) list  |
           ExtRec of string * extTsb |
           ExtCall of string ;; 

(*Examples*)
(*let g = TSBExtGuard(And(Not(SC(TSBClock "x", Less, 4)),Or(Not(DC (TSBClock "x", TSBClock "t",  LessEq, 7)), DC(TSBClock "t", TSBClock "x", Eq, 5))));;*)
