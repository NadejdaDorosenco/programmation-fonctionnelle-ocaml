(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)

open Types
open Read_polish
open Print_polish
open Eval_polish  
open Simpl_polish
open Vars_polish
open Sign_polish
  
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage:
  -reprint 
  -eval 
  -simpl 
  -vars 
  -sign\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] ->print_polish (simpl_polish (read_polish file))
  | [|_;"-vars";file|] -> vars_polish (read_polish file)
  | [|_;"-sign";file|] -> sign_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
