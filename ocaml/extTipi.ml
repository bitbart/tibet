(**
*******************************************************
**                                                   **
**      2) Extended Timed TSB - Extended static      **
**                                                   **
*******************************************************
**)

type tsb_ext_relation = ExtLess | ExtGreat | ExtLessEq | ExtGreatEq | ExtEq;;

type extGuard =   SC of tsb_clock * tsb_ext_relation * int              (*simple constraint: es:  t < 4*) 
                | DC of tsb_clock * tsb_clock * tsb_ext_relation * int  (*diagonal constraint: es: t - x < 4*) 
                | And of extGuard * extGuard 
                | Or of extGuard * extGuard 
                | Not of extGuard
								| True
								| False;;

type tsb_extGuard =  TSBExtGuard of extGuard;;

type extTsb = ExtNil | ExtSuccess |
           ExtIntChoice of (tsb_action * tsb_extGuard * tsb_reset * extTsb) list | 
           ExtExtChoice of (tsb_action *tsb_extGuard * tsb_reset * extTsb) list  |
           ExtRec of string * extTsb |
           ExtCall of string ;; 

(*Examples*)
let g = TSBExtGuard(And(Not(SC(TSBClock "x", ExtLess, 4)),Or(Not(DC (TSBClock "x", TSBClock "t", ExtLessEq, 7)), DC(TSBClock "t", TSBClock "x", ExtEq, 5))));;

let g1 = TSBExtGuard(And(SC(TSBClock "x", ExtLess, 4),DC (TSBClock "x", TSBClock "t", ExtLessEq, 7)));;

let g2 = TSBExtGuard(Or(SC(TSBClock "x", ExtEq, 4),DC (TSBClock "x", TSBClock "t", ExtGreatEq, 7)));;

let g3 = TSBExtGuard(Or(SC(TSBClock "t", ExtEq, 4),
                  Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)))
          );;

let g4 = TSBExtGuard(Or( 
                         Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),
                         SC(TSBClock "t", ExtEq, 4)   
                        )              
          );;

let g5 = TSBExtGuard(And( 
                         Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),
                         SC(TSBClock "t", ExtEq, 4)   
                        )              
          );;

let g6 = TSBExtGuard(Or( 
                         And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),
                         SC(TSBClock "t", ExtEq, 4)   
                        )              
          );;

let g7 = TSBExtGuard(Or(SC(TSBClock "t", ExtEq, 4),
                  And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)))
          );;


(* t = 4 | ( x = 5 && s = 6)*)
let gA = TSBExtGuard(Or(SC(TSBClock "t", ExtEq , 4), And(SC(TSBClock "x", ExtEq, 5), SC (TSBClock "s", ExtEq, 6))));;

(* (t = 4 |  x = 5) && s = 6*)
let gB = TSBExtGuard(And(  Or(SC(TSBClock "t", ExtEq , 4),SC(TSBClock "x", ExtEq, 5)),  SC (TSBClock "s", ExtEq, 6)));;
