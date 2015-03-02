(** 
 ********************************************************************************
 **																																						 **
 **				ERRORS (0): Supports error handling.																 **
 **																																						 **
 ********************************************************************************
 **)


(*--------------------------------------------------
    OCAML TOPLEVEL IMPORTS (for Eclipse only)

#load "str.cma";;
#load "unix.cma";;
#load "xml-light.cma";;
#load "dynlink.cma";;
#load "camlp4o.cma";;
open Printf;;
--------------------------------------------------*)


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
let _ERR_017 = "ERR_017: An action cannot have an empty identifier.";;
let _ERR_018 = "ERR_018: Missing a valid action name after symbol '!'"
let _ERR_019 = "ERR_019: Missing 'E' after 'R' in REC declaration.";;
let _ERR_020 = "ERR_020: Missing 'C' after 'E' in REC declaration.";;
let _ERR_021 = "ERR_021: Recursive variables must be enclosed with single quotes (e.g. REC 'x'[...])";;
let _ERR_022 = "ERR_022: Missing variable name in REC." ;;
let _ERR_023 = "ERR_023: Error when parsing a guard after '{' symbol." ;;
let _ERR_024 = "ERR_024: A clock id cannot have an empty identifier.";;
let _ERR_025 = "ERR_025: Calling an undefined recursive block.";;
let _ERR_026 = "ERR_026: Syntax error: unexpected char sequence after '}'. Maybe you missed a '.' or a '+' or a '&' or a ')' after: ";;
let _ERR_027 = "ERR_027: Invalid operator in automaton guard.";;
let _ERR_028 = "ERR_028: Invalid element found in internal choice (only branches starting with an internal action are allowed).";;
let _ERR_029 = "ERR_029: Invalid element found in external choice (only branches starting with an external action are allowed).";;
let _ERR_030 = "ERR_030: Invalid element found in XML.";;
let _ERR_031 = "ERR_031: Not valid contract XML file.";;
let _ERR_032 = "ERR_032: Invalid char sequence found in your contract.";;
let _ERR_033 = "ERR_033: Invalid char sequence found in your contract.";;
let _ERR_034 = "ERR_034: Invalid char sequence found in your contract.";;
let _ERR_035 = "ERR_035: Invalid char sequence found in your contract.";;
let _ERR_036 = "ERR_036: Invalid char sequence found in your contract.";;
let _ERR_037 = "ERR_037: Not a valid XML contract!";;
let _ERR_038 = "ERR_038: Syntax error: unexpected char sequence after ']'. Maybe you missed a '+' or a '&' or a ')' after: ";;
let _ERR_039 = "ERR_039: Syntax error: unexpected char sequence after '?'. Maybe you missed an action at: ?";;
let _ERR_040 = "ERR_040: Syntax error: unexpected char sequence after '!'. Maybe you missed an action at: !";;
let _ERR_041 = "ERR_041: Syntax error: probably you miss a '!' or a '?' before a branch label.";;
let _ERR_042 = "ERR_042: Missing a valid operand before symbol '.'. Sequence requires two valid operands.";;
let _ERR_043 = "ERR_043: Missing a valid operand after symbol '.'. Sequence requires two valid operands.";;
let _ERR_044 = "ERR_044: Missing a valid operand before symbol '+'. Internal choice requires two valid operands.";;
let _ERR_045 = "ERR_045: Missing a valid operand after symbol '+'. Internal choice requires two valid operands.";;
let _ERR_046 = "ERR_046: Missing a valid operand before symbol '&'. External choice requires two valid operands.";;
let _ERR_047 = "ERR_047: Missing a valid operand after symbol '&'. External choice requires two valid operands.";;
let _ERR_048 = "ERR_048: Syntax error: unbalanced round brackets, maybe you missed a ')'.";;
let _ERR_049 = "ERR_049: Syntax error: unbalanced round brackets, maybe you missed a '('.";;
let _ERR_050 = "ERR_050: Syntax error: unbalanced square brackets, maybe you missed a ']'.";;
let _ERR_051 = "ERR_051: Syntax error: unbalanced square brackets, maybe you missed a '['.";;
let _ERR_052 = "ERR_052: Syntax error: unexpected char sequence after a recursive variable at: ";;
let _ERR_053 = "ERR_053: Missing a valid clock value after symbol '=' in at least one guard field.";;

let _ERR_999 = "ERR_999: The contract contains an unhandled syntax error. Check the syntax tutorial.";;


(* LIST OF MONITOR ERRORS *)
let _ERR_100 = "ERR_100: Action not found.";;
let _ERR_101 = "ERR_101: Wrong pid.";;
let _ERR_102 = "ERR_102: Unbound id: Call.";;
let _ERR_103 = "ERR_103: Not a valid contract to dualize.";;


(* LIST OF PYTHON ERRORS *)
let _ERR_200 = "ERR_200: Clock not declared in this context.";;
let _ERR_201 = "ERR_201: Error during equivalence operation.";;