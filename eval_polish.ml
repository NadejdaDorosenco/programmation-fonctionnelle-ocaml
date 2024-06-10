open Types
(*Cette fonction permet d'évaluer un opérateur en utilisant un fonction qui prend deux parametres et fait l'opération necessaire (+,-,*,/,%)  *)
let eval_op (op:op) = match op with 
  | Add -> fun x y -> x + y
  | Sub -> fun x y -> x - y
  | Mul -> fun x y -> x * y
  | Div -> fun x y -> x / y
  | Mod -> fun x y -> x mod y
;;
(*Cette fonction permet d'evaluer une comparaison  en utilisant un fonction qui prend deux parametres et fait la comparaison necessaire *)
let eval_comp (comp:comp)  = match comp with 
  | Eq -> fun x y -> x = y
  | Ne -> fun x y -> x <> y
  | Lt -> fun x y -> x < y
  | Le -> fun x y -> x <= y
  | Gt -> fun x y -> x > y
  | Ge -> fun x y -> x >= y
;;
(*Cette fonction evalue l'expression en faisant un match (Num()|Var()|Op()) et renvoie la valeur associée *)
let rec eval_expr (expr:expr) (env:(string*int)list) :int = match expr with
  | Num x -> x
  | Var x -> List.assoc x env
  | Op(op,exp,exp') -> eval_op (op) (eval_expr  exp env ) (eval_expr  exp' env )
;;
(*Cette fonction evalue la condition con et renvoie un bool*)
let  eval_cond  (cond:cond) (env:(string*int)list) :bool = match cond with
| (expr,comp,expr') -> eval_comp (comp) (eval_expr expr env ) (eval_expr expr' env )
;;

let eval_block (block:block) (env:(string*int)list)  :unit =
  let a =
    let rec instruction block env  = match block with 
      | []->env 
      | (pos,ins)::r->match ins with 
                     | Set (name,expr)->if List.mem_assoc name env=false then
                                        let a =(env@[(name,eval_expr expr env)]) in 
                                        instruction r a 
                                        else let b= List.remove_assoc name env in 
                                        instruction r (b@[(name,eval_expr expr env)])
                     | Read name -> (print_string(name^" ?"));
                                     let a = env@[(name,read_int())] in
                                     instruction r a 
                     | Print expr-> print_int (eval_expr (expr) (env));
                                    print_string("\n"); 
                                    instruction r env 
                     | If(cond,block,block')-> if (eval_cond cond env) then
                                               let a =instruction block env in
                                               instruction r a 
                                               else let b =instruction block' env in
                                               instruction r b 
                     | While(cond,block)->if eval_cond cond env then
                                          let x = instruction block env in 
                                          instruction ([(pos,ins)]@r) x 
                                          else instruction r env
    in instruction block env
  in ()            
;;


let eval_polish (p:program) : unit = eval_block p [] 
;;