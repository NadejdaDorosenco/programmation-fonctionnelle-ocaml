open Types

module Names = Set.Make(String);;

(* Fonction qui affiche les variables d'un Names.t *)
let print_vars (vars : Names.t) : unit = 
  let rec print_all_vars(var_list : string list) : unit = 
    match var_list with 
    | [] -> ()
    | h :: q -> print_string(h); print_string " "; print_all_vars(q) 
  in print_all_vars (Names.elements vars)
;;

let rec print_exp exp = match exp with
  | Num x -> string_of_int x
  | Var x -> x
  | Op (op,exp,exp') -> print_exp exp
;;
             
(* Fonctions récursives mutuelles qui renvoie un Names.t contenant le nom de toutes les variables *)
let rec vars_inst (ins : instr) : (Names.t) = 
  match ins with 
  | Set (name,expr) -> Names.add name Names.empty
  | Read name -> Names.add name Names.empty 
  | If(cond,block,block') -> Names.union (all_vars block) (all_vars block')
  | While(cond,block) -> all_vars block
  | _ -> Names.empty 

and  all_vars (block : block) : (Names.t) = 
  match block with 
  | (pos,ins)::r -> Names.union (vars_inst (ins)) (all_vars (r)); 
  | []-> Names.empty
;; 

(* Fonction permettant d'ajouter une variable au Names.t *)
let add_var s init_vars : Names.t = Names.add s init_vars;;

(* Fonctions récursives mutuelles qui renvoie un Names.t contenant le nom de toutes les variables initialisés *)
let rec vars_init (block : block) (init_vars : Names.t) : Names.t =
  match block with
  | [] -> init_vars
  | (pos,ins)::r -> match ins with
    | Set(name, expr) -> let init_vars = add_var name init_vars in
        vars_init r init_vars
    | Read name -> let init_vars = add_var name init_vars in
        vars_init r init_vars
    | If(cond,block,block') -> Names.inter (vars_init block init_vars) (vars_init block' init_vars)
    | While(cond, block) -> init_vars
    | Print name -> if Names.mem (print_exp name) (init_vars) then vars_init r init_vars
        else Names.inter (Names.remove (print_exp name) init_vars) (vars_init r init_vars);; 

(* Renvoie un Names.t contenant le nom de toutes les variables non initialisés grace aux deux fonctions définis précedemment. *)
let vars_not_init (all_vars : Names.t) (init : Names.t) : Names.t = Names.diff all_vars init;;
            
let vars_polish (p:program) : unit = 
  let vars = all_vars(p) in
  print_vars(vars);print_string "\n";
  let init = vars_init (p) (Names.empty) in
  let not_init = vars_not_init (vars) (init) in
  print_vars(not_init); print_string "\n" 
;;