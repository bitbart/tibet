Description of files:
tipi.ml    : contains types used to describe TSB processes and UPPAAL automa 
extTipi.ml : contains types used to describe TSB processes and UPPAAL automa 
mapping.ml : contains functions to convert a extTSB/TSB process into an automa. 
             Main functions are located at the end of the file.
toXML.ml:  : contains functions to convert and UPPAAL automa into xml, 
             and also to write an xml to file.
test.ml    : contains examples 
extTest.ml : constains example for the new mapping

Examples: 
1) 

let r1 = [TSBClock "x";TSBClock "t"];;
let r2 = [TSBClock "t"];;
let g1 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,6), (SC(TSBClock "x",ExtLessEq,12))));;
let g2 = TSBExtGuard (And (SC(TSBClock "x",ExtLessEq,10), (SC(TSBClock "x",ExtLessEq,22))));;
let t  = ExtIntChoice [(TSBAction "c", g1, TSBReset r1, ExtSuccess); 
                       (TSBAction "d", g2, TSBReset r2, ExtSuccess)];;
buildAutomatonMain t "p";;

2)
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



