

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
let g2 = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let r2 = CO2Reset [CO2Clock "t"; CO2Clock "x"];;

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

(********************************************************)
(*                                                      *)
(*  ES2: dual                                           *)
(*                                                      *)
(********************************************************) 
let g3 = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let r3 = CO2Reset [CO2Clock "t"; CO2Clock "x"];;

let p3 = IntChoice [ (CO2Action "a", g3, r3 , Success)];;
let q3 = ExtChoice [ (CO2Action "a", g3, r3 , Success)];;
let lta = co2_mapping p3 q3;;
writeToFile lta;




