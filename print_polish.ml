open Types

let print_op op = match op with 
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
;;

let print_comp comp=match comp with 
| Eq -> "="
| Ne -> "<>"
| Lt -> "<"
| Le -> "<="
| Gt -> ">"
| Ge -> ">="
;;

let rec print_exp exp = match exp with
| Num x -> string_of_int x
| Var x -> x
| Op (op,exp,exp') -> print_op  op ^ " " ^ print_exp exp ^ " " ^ print_exp exp'
;;

let  print_cond cond= match cond with
| (exp,comp,exp') -> print_exp exp ^ " " ^ print_comp comp ^ " " ^ print_exp exp'
;;

let print_space i =
  let rec aux i s = match i with 
  | 0 -> s
  | x -> aux (i-1) (s^"  ")
  in aux i ""
;;

(*fonction faite avec les indentations  *)
let rec print_inst ins ind = match ins with 
  | Set (name,expr) -> print_space ind ^ name ^ " := " ^ print_exp expr ^ "\n"
  | Read name -> print_space ind ^ "READ " ^ name ^ "\n"
  | Print expr -> print_space ind ^ "PRINT " ^ print_exp expr ^ "\n"
  | If(cond,block,block') -> print_space (ind) ^ "IF " ^ print_cond cond ^ "\n"  ^
                             print_block block (ind+1) ^ print_space (ind) ^ "ELSE \n" ^ 
                             print_block block' (ind+1) 
  | While(cond,block) -> print_space (ind) ^ "WHILE " ^
                         print_cond cond ^
                         "\n" ^ print_block (block) (ind+1)

and  print_block block ind  = match block with 
| [] -> ""
| (pos,ins)::r -> print_inst (ins) (ind) ^ print_block (r) (ind) 
;;
let print_polish (p:program) : unit = print_string(print_block (p) 0) ;;