(** ************************************************************************** **)
(**                                                                            **)
(**  	TOXML (3): offers functions which generate xml uppaal templates          **)
(**									     																										   **)
(** ************************************************************************** **)

(* 
  ----------------------------------------------------
   OCAML TOPLEVEL IMPORTS (for Emacs only)

		#use "mapping.ml";; 
                open Printf;;
  ----------------------------------------------------
*)

(* Inclusions to be used when compiling with makefile - DO NOT COMMENT THE FOLLOWING LINES *)
open Printf;;
open Tipi;;
open Mapping;;
open Printf;;


let escape g =
	let ampregex = Str.regexp "&" in 
	let g' = Str.global_replace ampregex "&amp;" g in
	let ltregex = Str.regexp "<" in
	Str.global_replace ltregex "&lt;" g' 
;;

let escape_id d = 
	let drexp = Str.regexp "(" in
	let d' = Str.global_replace drexp "" d in
	let drexp = Str.regexp ")" in
	Str.global_replace drexp "" d'
;;

(*******************Writing Header **********************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let write_header = 
	"<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd'>\n<nta>\n" 
;;

let rec write_vars vars = match vars with 
[] -> ""
| hd::tl -> "bool "^hd^"= false; \n"^(write_vars tl)
;;

let rec write_clocks clocks = match clocks with 
[] -> ""
| (Clock c)::tl -> "clock "^c^"; \n"^(write_clocks tl)
;;

let rec write_chans labels = match labels with 
[] -> ""
| (Label l)::tl -> "urgent chan  "^l^"; \n"^(write_chans tl)
;;

let rec write_procs procs = match procs with 
[] -> ""
| (name, body)::tl -> "void "^name^"{\n"^(escape body)^"}\n\n"^(write_procs tl)
;;

let write_declarations lta = 
	"<declaration>// Place global declarations here.\n" ^
	(write_vars (List.fold_right (fun  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) s -> addSetSet gVars s) lta []))^"\n"^
        (write_clocks (List.fold_right (fun  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) s -> addSetSet gClocks s) lta []))^"\n"^
        (write_chans (List.fold_right (fun  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) s -> addSetSet  labels s) lta []))^"\n"^
       	"</declaration>\n"
;;

let write_local_declarations  (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = 
	"<declaration>// Place local declarations here.\n" ^
        (write_vars  vars)^"\n"^
        (write_clocks clocks)^"\n"^
        (write_procs procs)^"\n"^
	"</declaration>\n"
;;
(*******************Writing Transitions******************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
 
let write_transition  (Edge(Loc source, Label lbl, guard, reset, Loc target)) = 	
       	"<transition>\n" ^
	"<source ref=\""^ escape_id source ^"\"/>\n" ^
	"<target ref=\""^ escape_id target ^"\"/>\n" ^
	(if guard = "" then "" else ("<label kind=\"guard\">" ^ escape guard ^ "</label>\n")) ^
	(if lbl = "" then "" else ("<label kind=\"synchronisation\">" ^ lbl ^ "</label>\n")) ^
	(if reset = "" then "" else ("<label kind=\"assignment\">" ^ reset ^ "</label>\n")) ^
	"</transition>\n"	
;;

let write_transitions (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs))  = 
                                List.fold_right (fun x y -> (write_transition x)^y) edges  "";;

(*******************Writing Locations********************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 

let rec getInv l id = match l with 
[] -> ""
| (name, inv)::tl -> if name = id then inv else (getInv tl id)
;;

let rec isCommitted  l id = match l with 
[] -> false
| name::tl -> if name = id then true  else (isCommitted tl id)
;;

let write_location id committed inv = 	
        "<location id=\"" ^ escape_id id ^  "\">" ^
	"<name>" ^ escape_id id ^"</name>"^
        (if (getInv inv id)="" then "" else ("<label kind=\"invariant\">"^ escape (getInv inv id) ^"</label>"   ))^ 
        (if (isCommitted committed  id) then "<committed/>" else "")  ^
        " </location>\n";;

let rec write_locations_rec locs committed inv= match locs with
 []-> ""
| Loc id ::tl -> (write_location id committed inv)^ write_locations_rec tl committed inv;;

let write_locations (TimedAutoma (name, locs, initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = write_locations_rec locs committed inv;;	

let write_initLocation (TimedAutoma (name, locs, Loc initial, labels, edges,  inv,  clocks, gClocks,  committed, vars, gVars, procs)) = 
         "<init ref=\""^ escape_id  initial^ "\"/>\n";;


(*******************Writing Template *********** ********)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let getTemplateName (TimedAutoma (name, locs, Loc initial, labels, edges,  inv,  clocks, gClocks, committed, vars, gVars, procs)) = name;;

let write_template ta  = 
       	"<template>\n<name> Firing_"^ getTemplateName ta ^"</name>\n" ^
	write_local_declarations ta  ^  
	(write_locations ta) ^ (write_initLocation ta)  ^ (write_transitions ta) ^ "</template>\n"
     ;;

let witeTemplateName id = (String.uppercase id)^"Temp";;

(*******************Writing System Instantiation ********)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let rec write_systemDecRec lta = match lta with 
      []-> ""
   |  hd::tl -> (witeTemplateName(getTemplateName hd))^ " =  Firing_"^(getTemplateName hd)^"();\n"^(write_systemDecRec tl);;


let rec write_systemUseRec lta = match lta with 
      []-> "" 
   |  hd::hd2::tl -> (witeTemplateName(getTemplateName hd))^", "^(write_systemUseRec (hd2::tl) )
   |  hd::tl -> (witeTemplateName(getTemplateName hd))^"; \n";;


let write_system lta = 
  "<system>\n// Place template instantiations here.//\n" ^
  (write_systemDecRec lta)^
  "\n//List one or more processes to be composed into a system.\n"^
  "system "^ (write_systemUseRec lta)^
  "</system>\n</nta>\n"
;;


(************************)
(*generate the xml file accepted by the Uppaal model checker*) 


let aut_toXML  lta = 
	(write_header )^
        (write_declarations lta)^
        (List.fold_right (fun x s-> (write_template x)^s) lta "\n")^
        (write_system lta);;

let aut_toXML_out lta = 
	print_string(write_header );
        print_string(write_declarations lta );
        List.iter (fun x -> print_string(write_template x)) lta;
        print_string(write_system lta);;

(*******************Writing System Instantiation ********)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************) 
let getLocName (Loc name) = name;;

(* let rec write_queryRec lta = match lta with  *)
(*       []-> ""  *)
(*     |  hd::tl ->  let t = (witeTemplateName(getTemplateName hd))  *)
(*                   in  t^"."^(getLocName (getInit hd))^" --> "^t^"."^(getLocName successLoc)^"\n"^(write_queryRec (tl) );; *)

let  write_queryRec lta = "A[] not deadlock";;

let query_toXML  lta = 
	(write_queryRec lta);;



(********************************************************************************************)
(*                                                                                          *)
(*                                     Writing to file                                      *)
(*                                                                                          *)
(***************************************************************************************** **)

(* writeTA takes a list of timed automata and write them down in a file named  filename.xml*)
let writeTA lta filename = 
  let message = aut_toXML lta in
  (* Write message to file *)
  let oc = open_out (filename^".xml") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;                (* flush and close the channel *)
;;

let writeTAstd lta = 
  let message = aut_toXML lta in
  (* Write message to file *)
  fprintf stdout "%s\n" message   (* write something *)   
;;

(* writeTA generates the query for compliance  of a list of timed automata and write them down in a file named  filename.q*)
let writeQuery lta filename = 
   let messageQ = query_toXML lta in
  (* Write message to file *)
  let oc = open_out (filename^".q") in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" messageQ ;   (* write something *)   
  close_out oc;                (* flush and close the channel *)
;;

(*alltogether*)
let writeToFile lta filename = 
     writeTA lta filename;   
     writeQuery lta filename; 
;;
