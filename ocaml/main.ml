(* This file will contain the code for the conversion from Json to abstract sintax *)

#use "toXML.ml";;

let filename = "prova";;

(* TODO in a few days *)

let c1 = ast_from_json (read_file ());;

let c2 = ast_from_json (read_file ());;

let lta = tsb_mapping c1 c2;;
writeToFile lta filename;;