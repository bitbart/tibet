

#use "toXML.ml";;


(********************************************************)
(*                                                      *)
(*               Example for syntax                     *) 
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*This is an example for understanding syntax*)

(*!a{t<10&&t>1&&t<60,t,x,z}.(!b + !c) | ?a{y<4,y}.(!e + !f{z})   : not compliant*)
let p = IntChoice [(TSBAction "a", TSBGuard [(TSBClock "t", Less, 10);
                                             (TSBClock "t", Great, 1);
                                             (TSBClock "t", Less, 60) ], TSBReset[TSBClock "t"; TSBClock "x"; TSBClock "z"] , 
                             IntChoice [(TSBAction "b", TSBGuard [], TSBReset[] , Success);
                                        (TSBAction "c", TSBGuard [], TSBReset[] , Success)] )];;

let q = ExtChoice [(TSBAction "a", TSBGuard [(TSBClock "y", Less, 4)], TSBReset[TSBClock "y"] , 
                             ExtChoice [(TSBAction "e", TSBGuard [], TSBReset[] , Success);
                                        (TSBAction "f", TSBGuard [], TSBReset[TSBClock "z"] , Success)] )];;

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

(*!a{t<10} | ?a  : compliant *)
let p = IntChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex05";;

(*!a{t<10} + !b{t<10} | a  : not compliant because missing b in q*)
let p = IntChoice [(TSBAction "a",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success);
                   (TSBAction "b",  TSBGuard [(TSBClock "t", Less, 10)], TSBReset[] , Success)];;
let q = ExtChoice [(TSBAction "a", TSBGuard[], TSBReset[] , Success)];;
let lta = tsb_mapping   p q;;
writeToFile lta "ex06";;

(*!a{t<10} |?a + ?b     : compliant *)
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








