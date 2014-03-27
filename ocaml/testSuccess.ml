open Printf;;

#use "toXML.ml";;


let file = "example";;


(********************************************************)
(*                                                      *)
(*  ES1: a b c d are fired whenever they want           *)
(*  A succeed when c is fired                                                 *)
(********************************************************) 
let en_1 = [   Enabling(  [], Event "a",  [] , 0,  10) ; 
                Enabling(  [], Event "b",  [] , 0,  10) ;
                Enabling(  [], Event "c",  [] , 0,  10) ;
                Enabling(  [], Event "d",  [] , 0,  10) ;
                Enabling(  [Event "c"], Event "s_A", [],  0, 1 )];;

let tes_1 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c"; Event "s_A" ], 
         en_1, 
         []
       )
;;
let lta = es_mapping tes_1;;


(********************************************************)
(*                                                      *)
(*  ES2: a b c d are fired whenever they want           *)
(*       c is in conflict with b                        *)
(*  A succeeds whether  c or b are  fired                *)
(********************************************************) 
let en_2 = [    Enabling(  [], Event "b",  [] , 0,  10) ;
                Enabling(  [], Event "c",  [] , 0,  10) ;
                Enabling(  [Event "c"], Event "s_A", [],  0, 1 );
                Enabling(  [Event "b"], Event "s_A", [],  0, 1 )];;

let tes_2 = TimedEventStructure 
       ( [ Event "b"; Event "c"; Event "s_A" ], 
         en_2, 
         [Conflict (Event "b", Event "c")]
       )
;;
let lta = es_mapping tes_2;;

(*******************Queries******************************)
(*                                                      *)
(*  ES7:   a enables b                                  *)
(*         b enables both  c and d                      *)
(*         There is a conflict between c and d          *)
(*         a, b, c enables e iff c happens  within  5 time units from b *)
(*         a, b, d enables e iff d happens  within  2 time units from d   *)
(*                                                           *)
(*         A succeeds  if e is fired                    *)
(********************************************************) 
let en_7 = [   Enabling(  [], Event "a",  [] , 0,  2) ; Enabling(  [Event "a"], Event "b",  [] , 0,  2) ; 
               Enabling(  [Event "b"], Event "c",  [] , 0,  2) ;
               Enabling(  [Event "b"], Event "d",  [] , 0,  2) ;
               Enabling(  [Event "a"; Event "b"; Event "c"], Event "e", 
                              [ TimeConstraint ((RelEvent "b", 0), Event "c", (RelEvent "b", TimeB 5)) ] ,  0,  1);
               Enabling(  [Event "a"; Event "b"; Event "d"], Event "e", 
                            [ TimeConstraint ((RelEvent "b", 0), Event "d", (RelEvent "b", TimeB 2)) ] ,  0,  1);
               Enabling(  [Event "e"], Event "s_A", [],  0, 1 )
           ];;

let tes_7 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c"; Event "e"; Event "s_A" ], 
         en_7, 
         [Conflict (Event "d", Event "c")]
       )
;;

let lta = es_mapping tes_7;;

(*******************Queries******************************)
(*                                                      *)
(*  ES7:   a enables b                                  *)
(*         b enables both  c and d                      *)
(*         There is a conflict between c and d          *)
(*         a, b, c enables e iff c happens  within  5 time units from b, and a happen after 1 time unit , and b happens after a *)
(*         a, b, d enables f iff d happens  within  2 time units from d   *)
(*                                                           *)
(*         A succeeds  if e is fired                    *)
(********************************************************) 
let en_8 = [   Enabling(  [], Event "a",  [] , 1,  3) ; Enabling(  [Event "a"], Event "b",  [] , 0,  2) ; 
               Enabling(  [Event "b"], Event "c",  [] , 0,  2) ;
               Enabling(  [Event "b"], Event "d",  [] , 0,  2) ;
               Enabling(  [Event "a"; Event "b"; Event "c"], Event "e", 
                              [ TimeConstraint ((RelEvent "b", 0), Event "c", (RelEvent "b", TimeB 5));
                                TimeConstraint ((None, 1), Event "a", (None, Infinity));
                                TimeConstraint ((RelEvent "a", 0), Event "b", (None, Infinity))],  0,  1);
               Enabling(  [Event "a"; Event "b"; Event "d"], Event "f", 
                            [ TimeConstraint ((RelEvent "b", 0), Event "d", (RelEvent "b", TimeB 2)) ] ,  0,  1);
               Enabling(  [Event "e"], Event "s_A", [],  0, 1 );
               Enabling(  [Event "f"], Event "s_A", [],  0, 1 )
           ];;

let tes_8 = TimedEventStructure 
       ( [ Event "d"; Event "a"; Event "b"; Event "c"; Event "e"; Event "f"; Event "s_A" ], 
         en_8, 
         [Conflict (Event "d", Event "c")]
       )
;;

let lta = es_mapping tes_8;;


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
