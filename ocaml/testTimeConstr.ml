open Printf;;

#use "toXML.ml";;


let file = "example";;

(********************************************************)
(*                                                      *)
(*  ES1: empty process with clocks         *)
(*                                                      *)
(********************************************************) 
let p1 = Success;
let lta = co2_mapping (p1, [CO2Clock "t"; CO2Clock "y"]) ( p1, [CO2Clock "t"; CO2Clock "x"]);;


(********************************************************)
(*                                                      *)
(*  ES2: internal choice                                *)
(*                                                      *)
(********************************************************) 
let g2 = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let r2 = CO2Reset [CO2Clock "t"; CO2Clock "x"];;
let p2 = IntChoice [(CO2Action "a", g2, r2 , Success); 
                   (CO2Action "b", g2, r2 , IntChoice[(CO2Action "c", g2, r2, Success)])];;

let lta = co2_mapping (p2, [CO2Clock "t"; CO2Clock "y"]) ( p2, [CO2Clock "t"; CO2Clock "x"]);;



(*******************Queries******************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let message = aut_toXML lta ;;

let () =
  (* Write message to file *)
  let oc = open_out (file^".xml") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;                (* flush and close the channel *)


(*******************Queries******************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 

let messageQ = query_toXML lta ;;


let () =
  (* Write message to file *)
  let oc = open_out (file^".q") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" messageQ ;   (* write something *)   
  close_out oc;                (* flush and close the channel *)


(****************************************************************************************************************) 


