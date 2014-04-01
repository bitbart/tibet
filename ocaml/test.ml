

#use "toXML.ml";;


(********************************************************)
(*                                                      *)
(*               Testing:                               *) 
(*               Standard examples                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)
(*we say compliant if both valuation expression go green*)
(*QTemp.l0_1 --> QTemp.f*)

(*1|1 compliant*)
let lta = co2_mapping Success Success;;
writeToFile lta "ex1";;

(*1| a      :not compliant  *)
let p = ExtChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let lta = co2_mapping  Success p;;
writeToFile lta "ex2";;

(*bar_a| 1  :not compliant*)
let p = IntChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let lta = co2_mapping   p Success;;
writeToFile lta "ex3";;

(*bar_a | a  :not compliant because of missing time boundaries*)
let p = IntChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let lta = co2_mapping   p q;;
writeToFile lta "ex4";;

(*bar_a{t<10} | a  : compliant because of  time boundaries*)
let p = IntChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let lta = co2_mapping   p q;;
writeToFile lta "ex5";;

(*bar_a{t<10} \intchoice bar_b{t<10} | a  : not compliant because missing b in q*)
let p = IntChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success);
                   (CO2Action "b",  CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a", CO2Guard[], CO2Reset[] , Success)];;
let lta = co2_mapping   p q;;
writeToFile lta "ex6";;

(*\bar a{t<10} | a \extchoice b     : compliant *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success);
                   (CO2Action "b",  CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let lta = co2_mapping   p q;;
writeToFile lta "ex7";;

(*\bar a{t<10} | a{t<4}      : not compliant: q is not willing to wait so much *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[] , Success)];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex8";;

(*\bar a{t<10} | a{t<14}      :  compliant  *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 14)], CO2Reset[] , Success)];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex9";;

(*\bar a{8<t && t<10} | a{3<t && t<14}      :  compliant  *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Great, 8);(CO2Clock "t", Less, 10)], CO2Reset[] , Success)];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Great, 3);(CO2Clock "t", Less, 14)], CO2Reset[] , Success)];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex10";;

(*P performs every action within 4tu from the previous one. Q waits only if everything ends in 20tu*)
(*\bar a{t<4}{t}.\bar b {t<4}{t}. \bar c{t<4}{t} | a{r<20}.b{r<20}.c{r<20}      :  compliant  *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"],
          IntChoice [(CO2Action "b", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"],  
             IntChoice [(CO2Action "c", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success)])])];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[] , 
          ExtChoice [(CO2Action "b",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[] , 
             ExtChoice [(CO2Action "c",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[] , Success)])])];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex11";;


(*P performs every action within 4tu . Q waits  20tu*)
(*\bar a{t<4}{t} + \bar b {t<4}{t} + \bar c{t<4}{t} | a{r<20} + b{r<20} + c{r<20}      :  compliant  *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success); 
                   (CO2Action "b", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success); 
                   (CO2Action "c", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success)];;
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success);
                   (CO2Action "b",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success);
                   (CO2Action "c",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success)];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex12";;

(*P performs every action within 4tu from the previous one. Q waits only if everything ends in 20tu*) 
(*\bar a{t<4}{t}.aa{t<4}{t} + \bar b {t<4}{t}.bb{t<4}{t} + \bar c{t<4}{t}.cc{t<4}{t} | *)
(*                 a{r<20}.aa{t<20} + b{r<20}.bb{r<20} + c{r<20}.cc{r<20}      :  compliant  *)
let p = IntChoice [(CO2Action "a", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"],
                         IntChoice [(CO2Action "aa", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success)]); 
                   (CO2Action "b", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], 
                         IntChoice [(CO2Action "bb", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success)]); 
                   (CO2Action "c", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"],  
                         IntChoice [(CO2Action "cc", CO2Guard [(CO2Clock "t", Less, 4)], CO2Reset[CO2Clock "t"], Success)])];; 
let q = ExtChoice [(CO2Action "a",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[], 
                         ExtChoice [(CO2Action "aa",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success)]);
                   (CO2Action "b",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  
                         ExtChoice [(CO2Action "bb",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success)]); 
                   (CO2Action "c",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[], 
                        ExtChoice [(CO2Action "cc",  CO2Guard [(CO2Clock "t", Less, 20)], CO2Reset[],  Success)])  ];;
                
let lta = co2_mapping   p q;;
writeToFile lta "ex13";;
