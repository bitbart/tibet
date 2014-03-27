open Printf;;

#use "toXML.ml";;


let file = "example";;


(********************************************************)
(*                                                      *)
(*  ES1: a b c d are fired whenever they want           *)
(*                                                      *)
(********************************************************) 
let en_1 = [   Enabling(  [], Event "a",  [] , 0,  10) ; 
                Enabling(  [], Event "b",  [] , 0,  10) ;
                Enabling(  [], Event "c",  [] , 0,  10) ;
                Enabling(  [], Event "d",  [] , 0,  10)   ];;

let tes_1 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c" ], 
         en_1, 
         []
       )
;;
let lta = mapping tes_1;;

(********************************************************)
(*                                                      *)
(*  ES2:                                                *)
(*         d enables a                                  *)
(*         c,d and b are fired whenever they  want      *)
(*                                                      *)
(********************************************************) 
let en_2 = [   Enabling(  [], Event "c",  [] , 0,  10) ; 
                Enabling(  [Event "d"], Event "a",  [] , 0,  10) ;
                Enabling(  [], Event "b",  [] ,  0,  10) ;
                Enabling(  [], Event "d",  [] , 0,  10)   ];;

let tes_2 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c" ], 
         en_2, 
         []
       )
;;
let lta = mapping tes_2;;


(********************************************************)
(*                                                      *)
(*  ES3:                                                *)
(*         d enables a                                  *)
(*         c,d enables b                                *)
(*         c and d are fired whenever they  want        *)
(*                                                      *)
(********************************************************) 
let en_3 = [   Enabling(  [], Event "c",  [] , 0,  10) ; 
                Enabling(  [Event "d"], Event "a",  [] , 0,  10) ;
                Enabling(  [Event "d"; Event "c"], Event "b",  [] ,  0,  10) ;
                Enabling(  [], Event "d",  [] , 0,  10)   ];;

let tes_3 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c" ], 
         en_3, 
         []
       )
;;
let lta = es_mapping tes_3;;

(********************************************************)
(*                                                      *)
(*  ES4:   a will occur between 0 and 2                 *)
(*         b will occur between 3 and 5                 *)
(*         a and b enable c only if a happens between 0 and 5 and b between 0 and 5. In this case c fires in 3 time units *)
(*         a and b enable d only if a happens before b. In this case d fires in 3 time units      *)
(*                                                      *)
(********************************************************) 
let en_4 = [   Enabling(  [], Event "a",  [] , 0,  2) ; 
                Enabling(  [], Event "b",  [] , 3,  5) ;
                Enabling(  [Event "a"; Event "b"], Event "c",  [TimeConstraint ((None,0), Event "a", (None, TimeB 5));
                                                                TimeConstraint ((None,0), Event "b", (None, TimeB 5))
                                                               ] ,  0,  3);
                Enabling(  [Event "a"; Event "b"], Event "d",  [ TimeConstraint ((RelEvent "a",0), Event "b", (None, Infinity))
                                                               ] ,  0,  3)   ];;

let tes_4 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c" ], 
         en_4, 
         []
       )
;;

let lta = es_mapping tes_4;;

(********************************************************)
(*                                                      *)
(*  ES5:   a will occur between 0 and 2                 *)
(*         b will occur between 3 and 5                 *)
(*         c will occur between 6 and 15                *)
(*         a and b and c enable d only if b happens between a and c; and a between 0 and 5. In this case d fires in 3 time units *)
(*                                                      *)
(*                                                      *)
(********************************************************) 

let en_5 = [   Enabling(  [], Event "a",  [] , 0,  2) ; 
                Enabling(  [], Event "b",  [] , 3,  5) ;
                Enabling(  [], Event "c",  [] , 6,  15) ;
                Enabling(  [Event "a"; Event "b"; Event "c"], Event "d",  
                                                              [ TimeConstraint ((None,0), Event "a", (RelEvent "b", TimeB 0));
                                                                TimeConstraint ((RelEvent "a" , 1), Event "b", (RelEvent "c", TimeB 0));
                                                                TimeConstraint ((RelEvent "b", 1), Event "c", (None, Infinity))
                                                              ] ,  0,  3)
                 ];;
let tes_5 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c" ], 
         en_5, 
         []
       )
;;

let lta = es_mapping tes_5;;


(********************************************************)
(*                                                      *)
(*  ES6:   a,b,c,d, will occur between 0 and 2 time unit         *)
(*         a and b enable e if a happens between 0 and 5 and b  between 0 and 4. In this case e fires within 1 to 4 time unit *)
(*         a c and d enable e if a,c,d happens between 0 and 5. In this case e fires within 1 to 5 time unit *)
(*         There is a conflict between b and c                                            *)
(*                                                      *)
(********************************************************) 


let en_6 = [   Enabling(  [], Event "a",  [] , 0,  0) ; Enabling(  [], Event "b",  [] , 0,  2) ; Enabling(  [], Event "c",  [] , 0,  2) ;
               Enabling(  [], Event "d",  [] , 0,  2) ;
               Enabling(  [Event "a"; Event "b"], Event "e", [ TimeConstraint ((None,0), Event "a", (None, TimeB 4));
                                                                TimeConstraint ((None, 0), Event "b", (None, TimeB 4))
                                                              ] ,  1,  4);
                Enabling(  [Event "a"; Event "c"; Event "d"], Event "e", [ TimeConstraint ((None,0), Event "a", (None, TimeB 5));
                                                                TimeConstraint ((None,0), Event "c", (None, TimeB 5));
                                                                TimeConstraint ((None, 0), Event "d", (None, TimeB 5))
							      ],  1,  5);

                 ];;

let tes_6 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c"; Event "e" ], 
         en_6, 
         [Conflict (Event "b", Event "c")]
       )
;;

let lta = es_mapping tes_6;;

(*******************Queries******************************)
(*                                                      *)
(*  ES7:   a enables b                                  *)
(*         b enables both  c and d                      *)
(*         There is a conflict between c and d          *)
(*         a, b, c enables e iff c happens  within  5 time units from b *)
(*         a, b, d enables e iff d happens  within  2 time units from d   *)
(*                                                           *)
(*                                                      *)
(********************************************************) 
let en_7 = [   Enabling(  [], Event "a",  [] , 0,  2) ; Enabling(  [Event "a"], Event "b",  [] , 0,  2) ; 
               Enabling(  [Event "b"], Event "c",  [] , 0,  2) ;
               Enabling(  [Event "b"], Event "d",  [] , 0,  2) ;
               Enabling(  [Event "a"; Event "b"; Event "c"], Event "e", 
                              [ TimeConstraint ((RelEvent "b", 0), Event "c", (RelEvent "b", TimeB 5)) ] ,  0,  1);
               Enabling(  [Event "a"; Event "b"; Event "d"], Event "e", 
                              [ TimeConstraint ((RelEvent "b", 0), Event "d", (RelEvent "b", TimeB 2)) ] ,  0,  1);
           ];;

let tes_7 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c"; Event "e" ], 
         en_7, 
         [Conflict (Event "d", Event "c")]
       )
;;

let lta = es_mapping tes_7;;


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
