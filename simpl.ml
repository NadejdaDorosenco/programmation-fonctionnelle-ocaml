open Types 
open Eval_polish

let simpl_add e1 e2 =match (e1,e2) with 
  |Num x,Num y->Num(x+y)
  |(Num 0,e2)  ->e2
  |(e1,Num 0)->e1
  |(e1,e2)->Op(Add,e1,e2)
;;
  
let simpl_sub e1 e2 =match (e1,e2) with 
  |Num x,Num y->Num(x-y)
  |(Num 0,e2)->e2
  |(e1,Num 0)->e1
  |(e1,e2)->Op(Sub,e1,e2)
;;

let simpl_mul e1 e2 =match (e1,e2) with 
  |Num x,Num y->Num(x*y)
  |(Num 0,e2)->Num 0
  |(e1,Num 0)->Num 0
  |(Num 1,e2)->e2
  |(e1,Num 1)->e1
  |(e1,e2)->Op(Mul,e1,e2)
;;

let simpl_div e1 e2 =match (e1,e2) with 
  |Num x,Num y->Num(x/y)
  |(Num 0,e2)->Num 0
  |(e1,Num 1)->e1
  |(e1,e2)->Op(Div,e1,e2)
;;

let simpl_mod e1 e2 =match (e1,e2) with 
  |Num x,Num y->Num(x mod y)
  |(Num 0,e2)->Num 0
  |(e1,Num 1)->Num 0
  |(e1,e2)->Op(Mod,e1,e2)
;;

let simpl_op op e1 e2 = match op with
  |Add->simpl_add e1 e2
  |Sub->simpl_sub e1 e2
  |Mul->simpl_mul e1 e2
  |Div->simpl_div e1 e2
  |Mod->simpl_mod e1 e2
;;

let simpl_exp expr=
  let rec aux expr= 
    match expr with 
      |Num x ->Num x
      |Var x->Var x
      |Op(op,e1,e2)->simpl_op (op) (aux e1) (aux e2)
  in aux expr
;;

let simpl_cond cond =
    match cond with 
      |(e1,comp,e2)->(simpl_exp e1,comp,simpl_exp e2)
;;

let simpl_block (block:program) newProgram :program=
  let rec simpl_ins block newProgram=
  match block with 
    |[]->List.rev(newProgram)
    |(pos,ins)::r-> match ins with 
        | Set (name,expr) -> simpl_ins r ((pos,Set(name,simpl_exp expr))::newProgram)
        | Read name -> simpl_ins r ((pos,Read name)::newProgram)
        | Print expr ->simpl_ins r ((pos,Print (simpl_exp expr))::newProgram)
        | If(cond,block,block') ->begin match simpl_cond cond  with 
          |(Num x,comp,Num y)->if(eval_comp (comp) (Num x) (Num y)) then  simpl_ins r block@newProgram
          else simpl_ins r block'@newProgram
          |(e1,comp,e2)->simpl_ins r ((pos,If(simpl_cond cond,simpl_ins (block) newProgram,simpl_ins (block') newProgram ))::newProgram)
        end                                                 
        | While(cond,block) ->begin match simpl_cond cond with 
          |(Num x,comp,Num y)->if(eval_comp (comp) (Num x) (Num y)) then simpl_ins r ((pos,While((Num x,comp,Num y),simpl_ins block newProgram))::newProgram)
          else simpl_ins r newProgram
          |(e1,comp,e2)->simpl_ins r ((pos,While(simpl_cond cond,simpl_ins block newProgram))::newProgram)
        end 
  in simpl_ins block newProgram
;;
    


let simpl_polish (p:program) : program = simpl_block p [] ;;








