open Types
let sign_add  = fun a b -> match a,b with
                          | Pos ,Pos -> [Pos]
                          | Neg ,Neg -> [Neg]
                          | Neg,Pos -> [Neg;Zero;Pos]
                          | Pos ,Neg -> [Neg;Zero;Pos]
                          | x,Zero->[x]
                          | Zero,x->[x]
                          | _,Error->[Error]
                          | Error,_->[Error]
;;
                      
let sign_sub = fun a b -> match a,b with
                           | Pos ,Pos -> [Pos;Zero;Neg]
                           | Neg ,Neg -> [Pos;Zero;Neg]
                           | Neg,Pos -> [Neg]
                           | Pos ,Neg -> [Pos]
                           | Zero,Pos ->[Neg]
                           | Zero,Neg ->[Pos]
                           | x,Zero ->[x]
                           | _,Error ->[Error]
                           | Error,_->[Error]
                          
;;

let sign_mul  =fun a b -> match a,b with
                          | Pos ,Pos -> [Pos]
                          | Neg ,Neg -> [Pos]
                          | Pos ,Neg -> [Neg]
                          | Neg,Pos -> [Neg]
                          | Zero,_ ->[Zero]
                          | _,Zero ->[Zero]
                          | Error,_ ->[Error]
                          | _,Error ->[Error]
;;

let sign_div   = fun a b -> match a,b with
                          | Pos ,Pos -> [Pos]
                          | Neg,Pos -> [Neg]
                          | Pos ,Neg -> [Neg]
                          | Neg ,Neg -> [Pos]
                          | Zero,_ ->[Zero]
                          | _,Zero ->[Error]
                          | Error,_ ->[Error]
                          | _,Error ->[Error]
;;

let sign_mod   = fun a b -> match a,b with
                          | x ,Error -> [Error]
                          | x ,Zero -> [Error]
                          | x ,_ -> [x]
;;


let sign_op op =
  match op with
     |Add -> sign_add               
     |Sub -> sign_sub 
     |Mul -> sign_mul 
     |Div -> sign_div 
     |Mod -> sign_mod 
;;

let sign_Eq =fun a b -> match a,b with
                       |_,Error-> false
                       |Error,_->false
                       |x,y->if x = y then true else false
;;

let sign_Ne =fun a b -> match a, b with
                       |_,Error-> false
                       |Error,_->false
                       |x,y->if x = y then false else true
;;

let sign_Lt =fun a b -> match a ,b with
                       |_,Error-> false
                       |Error,_->false
                       |_,Neg->false
                       |Neg,x->if x= Neg then false else true
                       |Pos,_->false
                       |x,Pos->if x = Pos then false else true
                       |Zero,x-> if (x=Zero || x=Neg )then false else true
;;

let sign_Le = fun a b -> match a ,b with
                       |_,Error-> false
                       |Error,_->false
                       |_,Neg->false
                       |Neg,_-> true
                       |Pos,_->false
                       |_,Pos-> true
                       |Zero,x-> if  x=Neg then false else true
;;


let sign_Gt  =fun a b -> match a, b with
                       |_,Error-> false
                       |Error,_->false
                       |x,Neg->if x=Neg then false else true
                       |Neg,_->false
                       |Pos,x->if x=Pos then false else true
                       |_,Pos->false
                       |Zero,x-> if (x=Zero || x=Pos )then false else true
                       

let sign_Ge =fun a b -> match a, b with
                       |_,Error-> false
                       |Error,_->false
                       |_,Neg-> true
                       |Neg,_->false
                       |Pos,_->true
                       |_,Pos->false
                       |Zero,x-> if x=Pos then false else true
;;

let sign_comp comp=match comp with 
| Eq -> sign_Eq
| Ne -> sign_Ne
| Lt -> sign_Lt
| Le -> sign_Le
| Gt -> sign_Gt
| Ge -> sign_Ge
;;

(* Cette fonction renvoie la liste acc qui correspond aux éléments de l sans doublons *)
let rec remove_duplicates l acc =
  match l with 
  |[] -> acc 
  |h::t->if (List.exists (fun x->if x=h then true else false) t) then remove_duplicates t acc else remove_duplicates t (h::acc) 
;;

(* Cette fonction prend en argument une opération op et deux listes de signes ls1 et ls2 et renvoie la liste de signe correspondante 
   selon l'opération entre les deux listes *)
let calcul_sign op ls1 ls2 =
  let rec loop1 op l1 l2 acc =
    match l1 with
    | [] -> remove_duplicates acc []
    | head :: tail -> loop1 op tail l2 (List.rev_append acc (loop2 op head l2 []))
  and loop2 op x l acc = 
    match l with 
    | [] -> acc
    | head :: tail -> loop2 op x tail (List.rev_append acc ((sign_op op) x head)) in
  loop1 op ls1 ls2 []
;; 


(* Cette fonction renvoie la liste de signes correspondante à expr *)
let rec expr_signList expr env  =
  match expr with
  | Num(x) -> if x > 0 then [Pos] else (if x < 0 then [Neg] else [Zero])
  | Var(x) -> (try Hashtbl.find env x with | Not_found -> [Error])
  | Op(op,exp1,exp2) -> calcul_sign op (expr_signList exp1 env) (expr_signList exp2 env) 
;;

(* read_sign remplace la list sign de la clé x par [Pos;Neg;Zero] dans env *)
let read_sign x env = Hashtbl.replace env x [Pos;Neg;Zero]  ;;

(* set_sign remplace la list sign de la clé var par la liste de signe de l'expression expr dans env *)
let set_sign var expr env = Hashtbl.replace env var (expr_signList expr env) ;;

let calcul_sign_cond comp  ls1 ls2 = 
  let rec loop1 comp l1 l2 bool_acc =
    match l1 with
    | [] -> bool_acc
    |head::tail-> loop1 comp tail l2 ( bool_acc  && (loop2 comp head l2 true))
  and loop2 comp x l bool_acc=
    match l with 
    |[]->bool_acc
    |head::tail->loop2 comp x tail ( bool_acc && ((sign_comp comp) x head)) in 
  loop1 comp ls1 ls2 true 
;;

(* Cette fonction permet de savoir si la condition est possible ou non *)
let cond_poss cond env  =
  match cond with
  | (expr1,comp,expr2) -> calcul_sign_cond comp (expr_signList expr1 env) (expr_signList expr2 env) ;;

(* Cette fonction renvoie les signes possibles pour une variable *)
let union_sign sl1 sl2 =
  let rec remove_duplicates l acc =
    match l with 
    | [] -> acc 
    | head :: tail -> if (List.exists (fun x->if x=head then true else false) tail) then remove_duplicates tail acc 
        else remove_duplicates tail (head::acc) in
  remove_duplicates(List.rev_append sl1 sl2) [] 
;;
let union  env env_block_if env_block_else  =
  Hashtbl.iter (fun cle value ->Hashtbl.replace env cle (union_sign (value) ( try( 
    Hashtbl.find env cle)with |Not_found -> value )) ) env_block_if;
  Hashtbl.iter (fun cle value ->Hashtbl.replace env cle (union_sign (value) ( try(
    Hashtbl.find env cle)with |Not_found -> value )) ) env_block_else
;; 

(*Cette fonction affiche les signes de la liste des signes*)
let  print_sign l=
  let rec print l l'=
    match l with 
    |[]->l'
    |head::tail->match head with 
      |Pos->print tail (l'^"+")
      |Neg->print tail (l'^"-")
      |Zero->print tail (l'^"0")
      |Error->print tail (l'^"!") in 
  print l "";;

(*Cette fonction affiche la variable (cle) et ses signes possibles *)
let print_env env =
  Hashtbl.iter(fun cle value->print_string (cle^" "^(print_sign value)^"\n")) env
;;

let verif_safe env =
  Hashtbl.fold (fun cle value init ->if ((List.mem Error value)=true)then (false&&init) else (true&&init) ) env true
;;

let print_safe env=if (verif_safe env )then print_string "safe \n"else print_string"pas safe \n"
;; 

let rec sign_block block env =
  match block with 
  |[]->()
  |(pos,ins)::tail->match ins with
    | Set(name,expr)-> (set_sign name expr env;sign_block tail env)
    | Read(name)-> (read_sign name env ; sign_block tail env) 
    | Print(expr)-> sign_block tail env 
    | If(cond,block,block')-> (let env_block_if = Hashtbl.copy env and env_block_else = Hashtbl.copy env in
                               (sign_block block env_block_if ;sign_block block' env_block_else ; 
                                union env env_block_if env_block_else ;
                                sign_block tail env ) )
    | While(cond,block)->(while (cond_poss cond env) do (sign_block block env )done ; 
                          sign_block tail env)
;;
let sign_polish (p:program) : unit =

  let env =  (Hashtbl.create 1000) in 
  (sign_block p env;
  print_env env ;
  print_safe env; )
;;