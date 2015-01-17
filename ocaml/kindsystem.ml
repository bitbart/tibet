#use "tipi.ml";;
#use "extTipi.ml";;
#use "python.ml";;

(*Da cancellare*)
let invReset (g:extGuard) (TSBClock x) = True;;
let past (g:extGuard) = True;;
let subtract (g:extGuard) (g':extGuard) = False;;
let equiv (g:extGuard) (g':extGuard) = true;;

type 'a partial = Some of 'a | None ;;

exception UndefinedVariable of string;;

type rec_env = RecEnv of (string -> extGuard partial);;

let emptyRecEnv = RecEnv (fun var -> None);;

let applyRecEnv (RecEnv env) var = match env var with 
    Some g -> g 
  | None -> raise (UndefinedVariable var);;

let bindRecEnv (RecEnv env) var guard = RecEnv (fun v -> if (v = var) then Some guard else env v);;

let invResetList (g:extGuard) (TSBReset list) = List.fold_left (fun g x -> Or(g , invReset g x)) True list;; 

let rec kindof env p = match p with
    ExtSuccess -> True
  | ExtNil -> False
  | ExtExtChoice(list) -> List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(And(guard,(invResetList (kindof env p') reset)))) list)
  | ExtIntChoice(list) ->  let gp = List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(guard)) list) and 
      ge =  List.fold_left (fun g x -> Or(g , x)) True (List.map (
    fun (act,(TSBExtGuard guard),reset,p') -> past(subtract guard (invResetList (kindof env p') reset))) list)
    in subtract gp ge
  | ExtCall var -> applyRecEnv env var 
  | ExtRec (var,p') -> let gFix env var p' = 
			 let rec f env var p' g = 
			   let g' = kindof (bindRecEnv env var g) p' 
			   in if (equiv g g') then g else f env var p' g'
			 in f env var p' True
		       in gFix env var p'
;;

let kindof p = kindof emptyRecEnv p;;
