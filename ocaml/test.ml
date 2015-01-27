
#use "toXML.ml";;
#use "cparser.ml";;

(*example*)
let p = IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success); (TSBAction "b", TSBGuard[], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success); (TSBAction "c", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex_prova";;


(*example*) (* !a{x > 2,x<1} # !b{x < 2,x > 1} *) (*?b*)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "r", LessEq, 4)], TSBReset[TSBClock "y"] , Success);
                    ( TSBAction "b", TSBGuard [(TSBClock "r", Eq, 6)], TSBReset[TSBClock "y"] , Success)
                  ];;
let q = ExtChoice [(TSBAction "a", TSBGuard [(TSBClock "r", LessEq, 4)], TSBReset[TSBClock "y"] , Success);
                    ( TSBAction "b", TSBGuard [(TSBClock "r", Eq, 6)], TSBReset[TSBClock "y"] , Success)
                  ];;
let lta = tsb_mapping   p q;;
getInvariants (List.nth lta 0);;
writeToFile lta "ex_111";;

(*counter example*)
(*!a{} | rec x.?a{}.x     :not compliant  *)
let p =  IntChoice [(TSBAction "a", TSBGuard[],  TSBReset[] , Success)];;
let q = Rec ("x", ExtChoice [(TSBAction "a", TSBGuard[],  TSBReset[] , Call "x")]);;
let lta = tsb_mapping  p q;;
writeToFile lta "exControesempio";;

(********************************************************)
(*                                                      *)
(*               Example for syntax                     *) 
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*This is an example for understanding syntax*)

(*!a{t<10&&t>1&&t<60,t,x,z}.(!b + !c) | ?a{y<4,y}.(!e + !f{z}) : not compliant*) 
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock
"t", Less, 10); (TSBClock "t", Great, 1); (TSBClock "t", Less, 60)],
TSBReset[TSBClock "t"; TSBClock "x"; TSBClock "z"], IntChoice
[(TSBAction "b", TSBGuard [], TSBReset[] , Success); (TSBAction "c",
TSBGuard [], TSBReset[] , Success)] )];;

let q = ExtChoice [(TSBAction "a", TSBGuard [(TSBClock "y", Less, 4)], TSBReset[TSBClock "y"] , 
                             ExtChoice [(TSBAction "e", TSBGuard [], TSBReset[] , Success);
                                        (TSBAction "f", TSBGuard [], TSBReset[TSBClock "z"] , Success)] )];;

let lta = tsb_mapping p q;;
writeToFile lta "ex50";;

let lta = tsb_mapping p q;;
writeToFile lta "ex50";;

(*rec x.!a{t>10, t}.x | rec x.?a{z<8,z}.x     :compliant  *)
let p = Rec ("x", ExtChoice [(TSBAction "a", TSBGuard[(TSBClock "t", Great, 10)],  TSBReset[TSBClock "t"] , Call "x")]);;
let q = Rec ("x", IntChoice [(TSBAction "a", TSBGuard[(TSBClock "z", Less, 8)],  TSBReset[TSBClock "z"] , Call "x")]);;
let lta = tsb_mapping  p q;;
writeToFile lta "ex51";;

(********************************************************)
(*                                                      *)
(*               Testing:                               *) 
(*               intervals                              *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*Attention: the b-guard has an empty interval so that it is uncorrect to take its*)
(*max bound as valid!!!!*)
(* !a{t<10 && t<4 && t<5 && t>2} + !b{t<10 && t<40 && t< 50 && t > 20} + !c{t<20 && t<14 && t<15 && t > 8}*)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10);
                                             (TSBClock "t", Less, 4);
                                             (TSBClock "t", Less, 5);
                                             (TSBClock "t", Great, 2); ], TSBReset[] , Success);
                   (TSBAction "b", TSBGuard [(TSBClock "t", Less, 10);
                                             (TSBClock "t", Less, 40);
                                             (TSBClock "t", Less, 50);
                                             (TSBClock "t", Great, 20); ], TSBReset[] , Success);
                   (TSBAction "c", TSBGuard [(TSBClock "t", Less, 20);
                                             (TSBClock "t", Less, 14);
                                             (TSBClock "t", Less, 15);
                                             (TSBClock "t", Great, 8); ], TSBReset[] , Success)];;

let lta = tsb_mapping p Success;;
writeToFile lta "ex30";;

(********************************************************)
(*                                                      *)
(*               Testing:                               *) 
(*               Standard examples                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*we say compliant if both valuation expression go green*)
(*QTemp.l0_1 --> QTemp.f*)

(*1|1: compliant*)
let lta = tsb_mapping Success Success;;
writeToFile lta "ex01";;

(*1| a      :not compliant  *)
let p = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping  Success p;;
writeToFile lta "ex02";;

(*!a| 1  :not compliant*)
let p = IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p Success;;
writeToFile lta "ex03";;

(*!a | ?a  :compliant*)
let p = IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex04";;

(*!a{t<10, t} | ?a{t<20, t}  : compliant *)
let p = IntChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[TSBClock "t"] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[(TSBClock "t", Less, 20)], TSBReset[TSBClock "t"] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex05";;

(*!a{t<10} + !b{t<10} | a  : not compliant because missing b in q*)
let p = IntChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex06";;

(*!a{t<10} |?a{t<10} + ?b{t<10}     : compliant *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex07";;

(*!a{t<10} | ?a{t<4}      : not compliant: q is not willing to wait so much *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 4)], TSBReset[] , Success)];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex08";;

(*!a{t<10} | ?a{t<14}      :  compliant  *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 14)], TSBReset[] , Success)];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex09";;

(*!a{8<t && t<10} | ?a{3<t && t<14}      :  compliant  *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Great, 8);(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Great, 3);(TSBClock "t", Less, 14)], TSBReset[] , Success)];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex10";;

(*P performs every action within 4tu from the previous one. Q waits only if everything ends in 20tu*)
(*!a{t<4,t}.!b {t<4,t}.!c{t<4,t} | ?a{r<20}.?b{r<20}.?c{r<20}      :  compliant  *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"],
          IntChoice [(TSBAction "b", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"],  
             IntChoice [(TSBAction "c", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success)])])];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[] , 
          ExtChoice [(TSBAction "b",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[] , 
             ExtChoice [(TSBAction "c",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[] , Success)])])];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex11";;


(*P performs every action within 4tu . Q waits  20tu*)
(*!a{t<4,t} + !b {t<4,t} + !c{t<4,t} | ?a{r<20} + ?b{r<20} + ?c{r<20}      :  compliant  *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success); 
                   (TSBAction "b", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success); 
                   (TSBAction "c", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success)];;
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success);
                   (TSBAction "c",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success)];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex12";;

(*P performs every action within 4tu from the previous one. Q waits only if everything ends in 20tu*) 
(*!a{t<4,t}.!aa{t<4,t} + !b{t<4,t}.!bb{t<4,t} + !c{t<4,t}.!cc{t<4,t} | *)
(*  ?a{r<20}.?aa{t<20} + ?b{r<20}.?bb{r<20} + ?c{r<20}.?cc{r<20}      :  compliant  *)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"],
                         IntChoice [(TSBAction "aa", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success)]); 
                   (TSBAction "b", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], 
                         IntChoice [(TSBAction "bb", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success)]); 
                   (TSBAction "c", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"],  
                         IntChoice [(TSBAction "cc", TSBGuard [(TSBClock "t", Less, 4)], TSBReset[TSBClock "t"], Success)])];; 
let q = ExtChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[], 
                         ExtChoice [(TSBAction "aa",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success)]);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  
                         ExtChoice [(TSBAction "bb",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success)]); 
                   (TSBAction "c",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[], 
                        ExtChoice [(TSBAction "cc",  TSBGuard [(TSBClock "t", Less, 20)], TSBReset[],  Success)])  ];;
                
let lta = tsb_mapping   p q;;
writeToFile lta "ex13";;

(*if you pay cash in 7 days or pay by credit card in 5, i will send you item in 10 from the day you paied*)




(********************************************************)
(*                                                      *)
(*               Testing:                               *) 
(*               Recursive examples                     *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*we say compliant if both valuation expression go green*)
(*QTemp.l0_1 --> QTemp.f*)

(*rec x.!a.x | rec x.?a.x     :compliant  *)
let p = Rec ("x", ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x")]);;
let q = Rec ("x", IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x")]);;
let lta = tsb_mapping  p q;;
writeToFile lta "ex20";;

(*rec x.(!a.x+!b) | rec x.(?a.x+?b)     : compliant  *)
let p = Rec ("x", IntChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x");
                             (TSBAction "b", TSBGuard[], TSBReset[] , Success)]);;
let q = Rec ("x", ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Call "x");
                             (TSBAction "b", TSBGuard[], TSBReset[] , Success)]);;
let lta = tsb_mapping  p q;;
writeToFile lta "ex21";;


(* rec x.(!a + !b{t<2}.?c.x | ?a + ?b{y<2}.!c{y>2}.?a :  compliant *)
let p = Rec("x", IntChoice [(TSBAction "a", TSBGuard[], TSBReset[], Success);
                            (TSBAction "b", TSBGuard[(TSBClock "t", Less, 2)], TSBReset[], 
                               ExtChoice [(TSBAction "c", TSBGuard[], TSBReset[], Call "x") ]    )]);;

let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[], Success);
                   (TSBAction "b", TSBGuard[(TSBClock "t", Less, 2)], TSBReset[], 
                        IntChoice[(TSBAction "c", TSBGuard[(TSBClock "t", Great, 2)], TSBReset[],
                           ExtChoice[(TSBAction "a", TSBGuard[], TSBReset[], Success   )]  )]  )];;

let lta = tsb_mapping  p q;;
writeToFile lta "ex23";;


(********************************************************)
(*                                                      *)
(*              Handling Monitor Errors                 *) 
(*               									                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
let p =  IntChoice[(TSBAction "a",  TSBGuard [], TSBReset[] , Success)];;
let q =  ExtChoice[(TSBAction "a",  TSBGuard [], TSBReset[] , Success)];;

(*correct interaction*)
let net1 = m_start p q;;

let net2 = m_step net1  (Fire ("A", Int (TSBAction "a" )));;
m_culpable net2;;
m_onDuty net1;;
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
let p =  IntChoice[(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 3)], TSBReset[TSBClock "t"] , 
										IntChoice[(TSBAction "b",  TSBGuard [(TSBClock "t", Less, 5)], TSBReset[TSBClock "t"] , Success)])];;
let q =  ExtChoice[(TSBAction "a",  TSBGuard [(TSBClock "tt", Less, 3)], TSBReset[] , 
                    ExtChoice[(TSBAction "b",  TSBGuard [(TSBClock "tt", Less, 8)], TSBReset[TSBClock "tt"] , Success)])];;

(* test interaction (updated) *)
let net1 = m_start p q;;
m_culpable net1;;
m_onDuty net1;;

let net2 = m_step net1 (Delay 2.);;
m_culpable net2;;
m_onDuty net2;;

let net3 = m_step net2  (Fire ("A", Int (TSBAction "a" )));;
m_culpable net3;;
m_onDuty net3;;


let net4 = m_step net3  (Fire ("B", Ext (TSBAction "a" )));;
m_culpable net4;;
m_onDuty net4;;

let net5 = m_step net4 (Delay 4.);;
m_culpable net5;;
m_onDuty net5;;


let net6 = m_step net5  (Fire ("A", Ext (TSBAction "a" )));;

let t0 = startTime;;
let t1 = incrTime t0 4.;;
applyTime t1 (TSBClock "x");;
let t2 = resetTime t1 [TSBClock "x";TSBClock "y" ];;
let t3 = incrTime t2 1.;;
applyTime t3 (TSBClock "x");;


(********************************************************)
(*                                                      *)
(*              Handling Python Errors                  *) 
(*               									                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

(* Original examples by Tiziana *)
let g1 = (And(SC(TSBClock "x", ExtLess, 4),DC (TSBClock "x", TSBClock "t", ExtLessEq, 7)));;
let g2 = (Or(SC(TSBClock "x", ExtEq, 4),DC (TSBClock "x", TSBClock "t", ExtGreatEq, 7)));;
let g3 = (Or(SC(TSBClock "t", ExtEq, 4), Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));;
let g4 = (Or(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g5 = (And(Or(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g6 = (Or(And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7)),SC(TSBClock "t", ExtEq, 4)));;
let g7 = (Or(SC(TSBClock "t", ExtEq, 4), And(SC(TSBClock "x", ExtEq, 4), SC (TSBClock "x",  ExtGreatEq, 7))));;
let gA = (Or(SC(TSBClock "t", ExtEq , 4), And(SC(TSBClock "x", ExtEq, 5), SC (TSBClock "s", ExtEq, 6))));;
let gB = (And(Or(SC(TSBClock "t", ExtEq , 4),SC(TSBClock "x", ExtEq, 5)),  SC (TSBClock "s", ExtEq, 6)));;

past g1;;
past g2;;
past g3;;
past g4;;
past g5;;
past g6;;
past g7;;
past gA;;
past gB;;

let clockX = TSBClock "x";;

invReset g1 clockX;;
invReset g2 clockX;;
invReset g3 clockX;;
invReset g4 clockX;;
invReset g5 clockX;;
invReset g6 clockX;;
invReset g7 clockX;;
invReset gA clockX;;
invReset gB clockX;;

subtract g1 g2;;
subtract g1 g3;;
subtract g1 g4;;
subtract g1 g5;;
subtract g1 g6;;
subtract g1 g7;;
subtract g1 gA;;
subtract g1 gB;;

equivalence g1 g1;;
equivalence g1 g2;;
equivalence g1 g3;;
equivalence g1 g4;;
equivalence g1 g5;;
equivalence g1 g6;;
equivalence g1 g7;;
equivalence g1 gA;;
equivalence g1 gB;;


let pythonOutput = "(1<c.s & c.s<=6 & c.s-c.x==1) | (2<=c.s & c.s<=6 & c.s-c.t==2)\n";;
let guard1 = String.sub pythonOutput 0 ((String.length pythonOutput)-1);;
let guard2 = remove_context guard1;;
let guard3 = replacing_uncomfortable guard2;;
let guard4 = reverse_guard guard3;;
let guard5 = python_infix_to_prefix guard4;;
let guard6 = adding_guard_separator guard5;;
let result = python_parser (Stream.of_string guard6);;


(********************************************************)
(*                                                      *)
(*              Handling Kindsystem Errors              *) 
(*               									                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
let a = TSBAction "a";;
let b = TSBAction "b";;
let emptyR = TSBReset[];;

let p1 = ExtExtChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 2)),emptyR,
ExtExtChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 1)),emptyR,ExtSuccess)])
];;

kindof p1;;
dualof p1;;
admitsCompliant p1;;(*should be true*)

let p2 = ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 2)),emptyR,
ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 1)),emptyR,ExtSuccess)])
];;

kindof p2;;
dualof p2;;
admitsCompliant p2;;(*should be false*)

let p3 = ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 1)),emptyR,
ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 1)),emptyR,ExtSuccess)])
];;

kindof p3;;
dualof p3;;
admitsCompliant p3;;(*should be true*)

let p4 = ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 2)),emptyR,ExtSuccess);
(b,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 1)),emptyR,
ExtExtChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLess, 0)),emptyR,ExtSuccess)])
];;

kindof p4;;
dualof p4;;
admitsCompliant p4;;(*should be false*)

let p5 = ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x", ExtLessEq, 2)),TSBReset[TSBClock "y"],
ExtExtChoice[(a,TSBExtGuard(And(SC(TSBClock "x", ExtGreatEq, 5),SC(TSBClock "y",ExtLessEq,2))),emptyR,ExtSuccess)])
];;

kindof p5;;
dualof p5;;
admitsCompliant p5;;(*should be false*)

let p6 = ExtRec ("X",
ExtExtChoice[(a,TSBExtGuard(And(SC(TSBClock "x", ExtLessEq, 1),SC(TSBClock "y",ExtLessEq,1))),emptyR,
ExtIntChoice[(a,TSBExtGuard(SC(TSBClock "x",ExtLessEq,1)),TSBReset[TSBClock "x"],ExtCall "X");
(b,TSBExtGuard(True),TSBReset[TSBClock "x";TSBClock "y"],ExtCall "X")])]);;

kindof p6;;
dualof p6;;
admitsCompliant p6;;(*should be false*)


(********************************************************)
(*                                                      *)
(*              Handling Parser Errors                  *) 
(*               									                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

(* There isn't an error and no error is detected. *)
parse_multiple_contracts "!a.!b.!c";;
parse_multiple_contracts "!a{}.!b{}.!c{}";;
parse_multiple_contracts "REC 'x'[!a+!b.'x']";;
parse_multiple_contracts "REC 'x'[!a.'x'+!b]";;
parse_multiple_contracts "REC 'x'[!a . 'x']";;


(* Error is detected correctly. *)
parse_multiple_contracts "!a{}.!p{}.!c{}!d{}.!e{}";;
parse_multiple_contracts "!Pippo{}.!x{}";;
parse_multiple_contracts "REC 'x'[!a+!b.'x']p";;
parse_multiple_contracts "!a{}.!b{}.?";;
parse_multiple_contracts "REC x[!a{}+!b{}.'x']";;
parse_multiple_contracts "REC 'x'[!a+!.'x']";;
parse_multiple_contracts "!a{}.?b{}.?";;
parse_multiple_contracts "!a.!a.";;
parse_multiple_contracts "REC 'x' [!a . 'x'";;
parse_multiple_contracts "REC 'x' !a . 'x']";;
parse_multiple_contracts "!a{}.!b{})";;
parse_multiple_contracts "REC 'x'[!a . 'x'string]";;
parse_multiple_contracts "REC 'x'[!a . 'x'p";;
parse_multiple_contracts "REC 'x'[!a . 'x'";;
parse_multiple_contracts "REC 'x'[!a . 'x'}";;
parse_multiple_contracts "REC 'x'[!a . 'x'$]";;


(* Error is correctly detected but it can be detected in a better way! *)
parse_multiple_contracts "!a.b";;                           (* Partially fixed. *)


(* Is error correctly detected? *)


(* Error is not detected correctly. *)
parse_multiple_contracts "REC 'x'[(!a . 'x')]";;						(* Error #1. Tailing tokens after ' must complain of round brackets. *)


(* An error is detected but it is not the real error. *)
parse_multiple_contracts "(((REC 'x')))[!y.'x']";;					(* Error #1. The error occurred is not correct, work in progress by Livio. *)
parse_multiple_contracts "!a.!b!c";;												(* Error #2. The error occurred is not correct, work in progress by Sebastian. *)


(* There is no error but an error occurred. *)


(* To test. *)
