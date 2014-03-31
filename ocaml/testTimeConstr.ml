

open Printf;;

#use "toXML.ml";;



(*******************************************************)
(*                                                      *)
(*               Writing to file                        *)
(*                                                      *)
(********************************************************) 
let file = "example";;

let writeTA lta = 
  let message = aut_toXML lta in
  (* Write message to file *)
  let oc = open_out (file^".xml") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;                (* flush and close the channel *)
;;

let writeQuery lta = 
   let messageQ = query_toXML lta in
  (* Write message to file *)
  let oc = open_out (file^".q") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" messageQ ;   (* write something *)   
  close_out oc;                (* flush and close the channel *)
;;

let writeToFile lta = 
     writeTA lta;   
     writeQuery lta;
;;


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
(*  ES2: internal choice                                *)
(*                                                      *)
(********************************************************) 
let ga = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let ra = CO2Reset [CO2Clock "t"; CO2Clock "x"];;

let gb = CO2Guard [(CO2Clock "t", Less, 3)];;
let rb = CO2Reset [CO2Clock "t"; CO2Clock "x"];;


let p2 = IntChoice [ (CO2Action "a", ga, ra , Success);  (CO2Action "b", gb, rb , Success) ];;

let p2 = IntChoice [ (CO2Action "a", g2, r2 , Success);
                     (CO2Action "b", g2, r2, 
                                    IntChoice [(CO2Action "b2", g2, r2 , Success); 
                                               (CO2Action "b3", g2, r2 , Success);  
                                               (CO2Action "b4", g2, r2 , Success)]);
                   (CO2Action "c", g2, r2, 
                                    IntChoice [(CO2Action "c2", g2, r2 , Success);
                                               (CO2Action "c3", g2, r2 , Success);
                                               (CO2Action "c4", g2, r2 , Success);
                                               (CO2Action "c5", g2, r2 , Success)]) ];;
let lta = co2_mapping p2 p2;;
writeToFile lta;

getInvariants(List.nth lta 1);;

(********************************************************)
(*                                                      *)
(*  ES2: dual                                           *)
(*                                                      *)
(********************************************************) 
let gP = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;

let gQ = CO2Guard [(CO2Clock "t", Less, 20)];;
let r = CO2Reset [];;

let p4 = IntChoice [ (CO2Action "a", gP, r , Success)];;
let q4 = ExtChoice [ (CO2Action "a", gQ, r , Success)];;
let lta = co2_mapping p4 q4;;
writeToFile lta;

getInvariants(List.nth lta 1);;


