
#use "extMapping.ml";;
#use "toXML.ml";;
(*#use "cparser.ml";;*)

(*example*)
let r1 = [TSBClock "x";TSBClock "t"];;
let r2 = [TSBClock "t"];;
let g1 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,6), (SC(TSBClock "x",ExtLessEq,12))));;
let g2 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,10), (SC(TSBClock "x",ExtLessEq,22))));;
let p  = ExtIntChoice [(TSBAction "c", g1, TSBReset r1, ExtSuccess); 
                       (TSBAction "d", g2, TSBReset r2, ExtSuccess)];;
let q  = ExtExtChoice [(TSBAction "c", g1, TSBReset r1, ExtSuccess); 
                       (TSBAction "d", g2, TSBReset r2, ExtSuccess)];;
let lta = extTsb_mapping   p q;;
writeToFile lta "ex_prova";;

let aut1 = buildAutomatonMain p "p";;
getLabels aut1;;

ta_to_string (aut1);;

let aut2 = buildAutomatonMain q "q";;
getLabels aut2;;

ta_to_string (aut2);;

(*example*)
let r1 = [TSBClock "x";TSBClock "t"];;
let r2 = [TSBClock "t"];;
let g1 = TSBExtGuard (And (SC(TSBClock "t",ExtLess,3), (SC(TSBClock "c",ExtGreat,6))));;
let q  = ExtExtChoice [(TSBAction "c", g1, TSBReset [], ExtSuccess); 
                       (TSBAction "d", g1, TSBReset [], ExtSuccess)];;

getOmega [(TSBAction "c", g1, TSBReset [], ExtSuccess); 
                       (TSBAction "d", g1, TSBReset [], ExtSuccess)];;

let lta = extTsb_mapping   p q;;
writeToFile lta "ex_prova";;

(* !pay{x<4;x}.?ship{x>7}*)

let r1 = [TSBClock "x"];;
let g1 = TSBExtGuard  (SC(TSBClock "x",ExtLess,4));;
let g2 = TSBExtGuard  (SC(TSBClock "x",ExtGreat,7));;
let p  = ExtIntChoice [(TSBAction "p", g1, TSBReset [], ExtExtChoice [(TSBAction "s", g2, TSBReset [], ExtSuccess)] )];;

let g3 = TSBExtGuard  (SC(TSBClock "x",ExtLess,5));;
let g4 = TSBExtGuard  (SC(TSBClock "x",ExtGreat,8));;
let q  = ExtExtChoice [(TSBAction "p", g3, TSBReset [], ExtIntChoice [(TSBAction "s", g4, TSBReset [], ExtSuccess)] )];;

getOmega [(TSBAction "c", g1, TSBReset [], ExtSuccess); 
                       (TSBAction "d", g1, TSBReset [], ExtSuccess)];;

let lta = extTsb_mapping   p q;;
getCommitted (List.nth lta 0);;

writeToFile lta "ex_prova";;

(* !pay{x<4;x}.?ship*)

let r1 = [TSBClock "x"];;
let g1 = TSBExtGuard  (SC(TSBClock "x",ExtLess,4));;
let g2 = TSBExtGuard  (SC(TSBClock "x",ExtGreat,7));;
let p  = ExtIntChoice [(TSBAction "p", g1, TSBReset [], ExtExtChoice [(TSBAction "s", TSBExtGuard True, TSBReset [], ExtSuccess)] )];;

let g3 = TSBExtGuard  (SC(TSBClock "x",ExtLess,5));;
let g4 = TSBExtGuard  (SC(TSBClock "x",ExtGreat,8));;
let q  = ExtExtChoice [(TSBAction "p", g3, TSBReset [], ExtIntChoice [(TSBAction "s", TSBExtGuard True, TSBReset [], ExtSuccess)] )];;

getOmega [(TSBAction "c", g1, TSBReset [], ExtSuccess); 
                       (TSBAction "d", g1, TSBReset [], ExtSuccess)];;

let lta = extTsb_mapping   p q;;
getCommitted (List.nth lta 0);;

writeToFile lta "ex_prova";;
