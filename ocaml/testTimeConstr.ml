

#use "toXML.ml";;



(********************************************************)
(*                                                      *)
(*              Testing                                 *)
(*                                                      *)
(********************************************************) 


(********************************************************)
(*                                                      *)
(*  ES1: empty process                                  *)
(*                                                      *)
(********************************************************) 
let p1 = Success;
let lta = co2_mapping p1 p1;;
writeToFile lta;;
 
(********************************************************)
(*                                                      *)
(*  ES2:                                                *)
(*                                                      *)
(********************************************************) 
let ga = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let ra = CO2Reset [CO2Clock "t"; CO2Clock "x"];;

let gb = CO2Guard [(CO2Clock "t", Less, 3)];;
let rb = CO2Reset [CO2Clock "t"; CO2Clock "x"];;


let p = IntChoice [ (CO2Action "a", ga, ra , Success);  (CO2Action "b", gb, rb , Success) ];;

let q = ExtChoice [ (CO2Action "a", ga, ra , Success);  (CO2Action "b", gb, rb , Success) ];;


let lta = co2_mapping p q;;
writeToFile lta "example";;

(********************************************************)
(*                                                      *)
(*  ES2:   bar_a{t<10}.bar_b | a{t<3}.b                 *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
(*non compliant guards*)
let gp = CO2Guard [(CO2Clock "t", Less, 10)];;
let gq = CO2Guard [(CO2Clock "t", Less, 3)];;                  

(*compliant guards*)
let gp = CO2Guard [(CO2Clock "t", Less, 5)];;
let gq = CO2Guard [(CO2Clock "t", Less, 7)];;  

let p = IntChoice [(CO2Action "a", gp, CO2Reset[] , 
             IntChoice [(CO2Action "b", CO2Guard[], CO2Reset[], Success)])];; 
let q = ExtChoice [(CO2Action "a", gq, CO2Reset[] , 
             ExtChoice [(CO2Action "b", CO2Guard[], CO2Reset[], Success)])];; 


let lta = co2_mapping p q;;
writeToFile lta "example";;
