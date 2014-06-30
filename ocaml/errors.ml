(** 
 ********************************************************************************
 **																																						 **
 **				ERRORS (0): supports error handling.																 **
 **																																						 **
 ********************************************************************************
 **)

(* LIST OF PARSER ERRORS *)
let _ERR_001 = "ERR_001: Invalid format for at least one action id or rec id  or clock id. Only a-z allowed.";;
let _ERR_002 = "ERR_002: Invalid format for at least one clock value. Only integer values allowed.";;
let _ERR_003 = "ERR_003: Missing a valid clock id after symbol ',' in at least one reset field.";;
let _ERR_004 = "ERR_004: Missing '}' or ',' in at least one reset field.";;
let _ERR_005 = "ERR_005: Missing a valid guard after symbol ',' in at least one guard field.";;
let _ERR_006 = "ERR_006: Missing a valid guard after symbol ',' in at least one guard field.";;
let _ERR_007 = "ERR_007: Missing a valid clock id after symbol ';' in at least one reset field.";;
let _ERR_008 = "ERR_008: Missing '}' or ';' after a guard.";;
let _ERR_009 = "ERR_009: Missing a valid clock value after symbol '<' in at least one guard field.";;
let _ERR_010 = "ERR_010: Missing a valid clock value after symbol '>' in at least one guard field.";;
let _ERR_011 = "ERR_011: Missing a valid guard in at least one guard field.";;
let _ERR_012 = "ERR_012: Missing a valid clock id after symbol ';' in at least one reset field.";;
let _ERR_013 = "ERR_013: Missing a valid reset in at least one reset field.";;
let _ERR_014 = "ERR_014: Missing a valid sequence or choice inside a REC[ ] declaration.";;
let _ERR_015 = "ERR_015: Missing '[' after at least one REC name.";;
let _ERR_016 = "ERR_016: Missing a valid action name after symbol '?'.";;
let _ERR_017 = "ERR_017: An action cannot have an empty identifier!";;
let _ERR_018 = "ERR_018: Missing a valid action name after symbol '!'"
let _ERR_019 = "ERR_019: Missing 'E' after 'R' in REC declaration.";;
let _ERR_020 = "ERR_020: Missing 'C' after 'E' in REC declaration.";;2
let _ERR_021 = "ERR_021: Missing '_' after 'C' in REC declaration.";;
let _ERR_022 = "ERR_022: Missing variable name in REC." ;;
let _ERR_023 = "ERR_023: Error parsing guard after '{' symbol." ;;
let _ERR_024 = "ERR_024: A clock id cannot have an empty identifier!";;
let _ERR_025 = "ERR_025: Calling an undefined recursive block.";;