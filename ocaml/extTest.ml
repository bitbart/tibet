
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
let lta = tsb_mapping   p q;;
writeToFile lta "ex_prova";;




