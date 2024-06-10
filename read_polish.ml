open Types

(* Cette fonction retourne l'indentation de la ligne passé en argument *)
let indentation (chaine : string) : int = 
  let rec countSpaces chaine compteur = 
    if compteur < String.length chaine && String.get chaine compteur = ' ' then countSpaces chaine (compteur + 1)
    else compteur
  in countSpaces chaine 0
;;

(* Permet de supprimer les espaces en début de ligne *)
let rec supprimeEspace (s : string) : string = 
  match String.get s 0 with
  | ' ' -> supprimeEspace (String.sub (s) (1) (String.length(s)-1))
  | _ -> s
;;

(* Extrait les tableau de charactère de chaque ligne du fichier et les couple avec le numero de ligne et l'indentation de celle-ci. 
  Cette fonction supprime aussi les commentaires et les espaces du à l'indentation *)
let extractLinesAndIndentation (filename:string) : list_lines =
  let ic = open_in filename in
  let read() =
    try Some (input_line ic) with End_of_file -> None in 
  let rec fillList list numLine = match read() with 
    | Some s -> if( not((List.hd (String.split_on_char (' ') (s))) = "COMMENT")) 
        then fillList ((numLine,indentation(s),String.split_on_char (' ') (supprimeEspace(s)))::list) (numLine+1) 
        else fillList list (numLine+1)
    | None -> close_in ic; List.rev(list) in
  fillList []  1
;;

let is_op (operator: string) : op = match operator with 
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "is_op only takes operator in argument"
;;

let is_int (s: string) : bool = 
  try (int_of_string s |> string_of_int) = s
  with Failure _ -> false;;

let is_key(s: string) : bool = 
  match s with
  | "READ" | "PRINT" | "IF" | "ELSE" | "WHILE" | "COMMENT" -> true
  | _ -> false;;

let is_var(s: string) : bool = if not(is_int(s)) && not(is_key(s)) then true 
  else false;;

(* Cette fonction prend une liste de string et retourne une expression, ainsi que la suite de la string : *)
let rec read_expr(l : string list) : (expr * string list) = 
  match l with 
  | [] -> failwith "read_expr []" 
  | h :: l -> if is_int (h) then (Num(int_of_string(h)), l) 
      else if h = "+" || h = "-" || h = "*" || h = "/" || h = "%" then
        let(e1, l') = read_expr l in 
        let(e2, l'') = read_expr l' in
        (Op(is_op(h), e1,e2), l'')
      else if is_var(h) then (Var(h), l) 
      else failwith "read_expr only takes expressions in argument";;

let is_comp(l : string) : bool = 
  match l with
  | "=" | "<>" | "<" | "<=" | ">" | ">=" -> true
  | _ -> false;;

let read_comp(l : string) : comp = 
  match l with
  | "=" -> Eq 
  | "<>" -> Ne 
  | "<" -> Lt 
  | "<=" -> Le 
  | ">" -> Gt 
  | ">=" -> Ge 
  | _ -> failwith "is_comp only takes operator in argument";;

(* Cette fonction prend une liste de mots et retourne une condition *)
let read_cond (l : string list) : cond = 
  let rec trouve_comp (expr1 : string list) (li : string list) = 
    match li with
    | [] -> failwith "read_cond []"
    | head :: tail -> if(not (is_comp(head))) then trouve_comp (expr1 @ [head]) (tail)
        (* On fait des match pour obtenir l'expression seule sans la suite de ligne : *)
        else let expr1 = match read_expr(expr1) with (h,_) -> h in
          let comp = read_comp(head) in
          let expr2 = match read_expr(tail) with (h,_) -> h in
          expr1, comp, expr2
  in trouve_comp [] l;;

(* get_block prend en argument : 
    _ un int correspondant à l'indentation d'une ligne dont le premier mot est while, if ou else (on l'a appelé ind_while pour donner un exemple)
    _ un list_lines (lines) correspondant à la liste de lignes dont on veut obtenir la sous liste de lignes et la suite de lignes à lire
    _ un list_lines (sous_block_while) correspondant à la sous liste de lines qu'on va venir compléter récursivement  
  et renvoie le sous bloc interne d'un while, if ou else (on l'obtient grace à l'indentation), ainsi que la suite du bloc lines de départ
  *)
let rec get_block (ind_while : int) (lines : list_lines) (sous_block_while : list_lines): (list_lines * list_lines) =
  match lines with 
  | [] -> List.rev(sous_block_while), []
  | (pos, ind, str) :: tail -> if (ind <= ind_while) then List.rev(sous_block_while),((pos, ind, str) :: tail)
      else get_block (ind_while) (tail) ((pos, ind, str) ::sous_block_while);;

(* read_block prend en argument : 
    _ un type list_lines
    _ un block
  et renvoie un block suivie d'un list_lines (qui si l'exécution se déroule bien sera à la fin une liste vide)

*)
let rec read_block (lines : list_lines) (acc_block : block) : (block * list_lines)  = 
  match lines with
  | [] -> (List.rev acc_block,lines)
  | (pos, ind, str) :: tail -> let (instr, reste_instr) = read_instruction ind str tail in
      read_block reste_instr ((pos,instr)::acc_block) 

(* read_instruction prend en argument : 
    _ l'indice d'une ligne (et notamment celle qui est passé en second argument)
    _ une ligne sous forme de string list (c'est-à-dire sans les commentaires et sans les espaces de début dû à l'indentation)
    _ une liste de lignes correspondant à la suite de lignes
  et renvoie une instruction et la suite de lignes à lire

*)

and read_instruction (ind : int) (line :  string list) (res_lig : list_lines) : (instr * list_lines) = 
  match line with
  | [] -> failwith "liste vide" 
  | "READ" :: tail -> (Read (List.hd tail),res_lig)
  | "PRINT" :: tail -> (Print (fst(read_expr tail)), res_lig) 
  | "WHILE" :: tail -> let cond = read_cond tail in
      let  (sous_block_while, suite_block) = get_block (ind) (res_lig) [] in
      let (expr_sous_block_while, instr_sous_block_while) = read_block sous_block_while [] in
      (While(cond, expr_sous_block_while), suite_block) 
  | "IF" :: tail -> let cond = read_cond tail in
      let (sous_bloc_if, suite_bloc) = get_block (ind) res_lig [] in
      let (expr_sous_block_if, instr_sous_block_if) = read_block sous_bloc_if [] in
      if (suite_bloc = []) then (If(cond, expr_sous_block_if, []), suite_bloc) 
      else
        begin
          match suite_bloc with 
          | [] -> (If(cond, expr_sous_block_if, []),suite_bloc)
          | (pos, ind, str) :: suite -> if(List.hd str = "ELSE") then 
                let (sous_block_else, suite_bloc_else) = get_block (ind) (List.tl suite_bloc) [] in 
                let (expr_sous_bloc_else, instr_sous_bloc_else) = read_block sous_block_else [] in 
                (If(cond, expr_sous_block_if, expr_sous_bloc_else), suite_bloc_else)
              else (If(cond, expr_sous_block_if, []),suite_bloc) 
        end
                 
  | var :: ":=" :: expr -> let exp = fst(read_expr expr) in
      (Set(var,exp), res_lig)
  | _ -> failwith ("Error read_instruction") ;;


(* read_polish prend en argument un fichier polish et renvoie un program (c'est-à-dire la syntaxe abstraite du fichier polish) *)
let read_polish (filename:string) : program = 
  let list = extractLinesAndIndentation(filename) in
  fst(read_block list []);;

