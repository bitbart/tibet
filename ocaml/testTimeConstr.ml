open Printf;;

#use "toXML.ml";;


let file = "example";;

(********************************************************)
(*                                                      *)
(*  ES1: empty process          *)
(*                                                      *)
(********************************************************) 
let p1 = Success;
let lta = co2_mapping (p1,[]) (p1,[]);;


(********************************************************)
(*                                                      *)
(*  ES1: empty process with clocks         *)
(*                                                      *)
(********************************************************) 
let p1 = Success;
let lta = co2_mapping (p1, [CO2Clock "t"; CO2Clock "y"]) ( p1, [CO2Clock "t"; CO2Clock "x"]);;

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

let t = CO2Clock "t";;
let g = CO2Guard [(CO2Clock "t", Less, 10); (CO2Clock "t", Great, 1)];;
let r = CO2Reset [CO2Clock "t"; CO2Clock "x"];;
(* \bar a, {g, r} \intsum \bar b, {g, r}. \bar a {g,r} *)
let p = IntChoice [(CO2Action "a", g, r , Success); 
                   (CO2Action "b", g, r , IntChoice[(CO2Action "a", g, r, Success)])];;
let p = ExtChoice [(CO2Action "a", g, r , Success)];;

let a = "s"::[];;
let p =  [(Action "a", g, r , Success); 
                   (Action "b", g, r , IntChoice[(Action "a", g, r, Success)])];;
List.map getGuard p;;
let lista = buildAutomaList (List.map getSuffix p);;
eliminateDuplicates(List.flatten (List.map getLocations lista));;

