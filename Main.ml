open Ast 
open Lexer
open Parser
open Semantics
open Lexing
open Types

let report_error filename lexbuf msg =
 let (b,e) = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
 let fc = b.pos_cnum - b.pos_bol + 1 in
 let lc = e.pos_cnum - b.pos_bol + 1 in
 Printf.eprintf "File \"%s\", line %d, characters %d-%d: %s\n" filename b.pos_lnum fc lc msg


(* main : unit -> unit *)
(* Analyse le contenu d'un fichier passé en paramètre ou l'entrée standard si aucun fichier n'est donné *)
(* Affiche OK si l'analyse syntaxique c'est bien passée et KO sinon *)
(* Dans le cas où l'analyse syntaxique c'est bien passé, lance l'analyse sémantique avec un environement d'évaluation initial vide *)
let main fichier =
  let input = open_in fichier in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    (* print_string (string_of_ast ast); *)
    let env = [] in 
    let mem = [] in
    (* Calcul de la valeur et de l'état de la mémoire *)
    referenceCounter := 0;
    counter := 0;
    let (valeur,memoire) = value_of_expr (ast,mem) env in
    (* Typage de l'expression *)
    let typ = type_of_expr ast env in
    close_in input;
    (typ,valeur,memoire)
  with
  | Lexer.Error s ->
      report_error fichier filebuf "lexical error (unexpected character).";
      exit 2
  | Parser.Error ->
      report_error fichier filebuf "syntax error.";
      exit 2

let getValeur (_,v,_) = v;;
let getMem (_,_,m) = m;; 
let getType (t,_,_) = t;;
let pop l = match l with
[]->assert false (**usage int *)
|t::q-> t
(*let printValue fichier  = print_endline (string_of_mem (getMem (main fichier)))
in
printValue  "../../exemples/exemple-14.mml"*)
(* Tests de non regression *)
let%test _ = ( getValeur (main "../../exemples/exemple-00.mml") = (IntegerValue 3) )
let%test _ = ( getValeur (main "../../exemples/exemple-01.mml") = (IntegerValue (-8)))
let%test _ = ( getValeur (main "../../exemples/exemple-02.mml") = (IntegerValue 4))
let%test _ = ( getValeur (main "../../exemples/exemple-03.mml") = (IntegerValue 5))
let%test _ = ( getValeur (main "../../exemples/exemple-04.mml") = (IntegerValue 1))
let%test _ = ( getValeur (main "../../exemples/exemple-05.mml") = (IntegerValue 2))
let%test _ = ( getValeur (main "../../exemples/exemple-06.mml") = (IntegerValue 120))
let%test _ = ( getValeur (main "../../exemples/exemple-07.mml") = (IntegerValue 10))
let%test _ = ( getValeur (main "../../exemples/exemple-08.mml") = (IntegerValue 5))
let%test _ = ( getValeur (main "../../exemples/exemple-09.mml") = (FrozenValue (FunctionNode ("x",AccessNode "x"),[])) )
let%test _ = ( getValeur (main "../../exemples/exemple-11.mml") = (IntegerValue 120) )
let%test _ = ( getValeur (main "../../exemples/exemple-12.mml") = (IntegerValue 120) )



(******************Test effet de bord (valeur) **************************)
let%test _ = ( getValeur (main "../../exemples/exemple-13.mml") = NullValue)
let %test _ = ( getValeur (main "../../exemples/exemple-14.mml") = NullValue)
let %test _ = ( getValeur (main "../../exemples/exemple-15.mml") = IntegerValue 0)
let %test _ = ( getValeur (main "../../exemples/exemple-16.mml") = NullValue)
let %test _= ( getValeur (main "../../exemples/exemple-17.mml") = IntegerValue 2)
let %test _= ( getValeur (main "../../exemples/exemple-18.mml") = NullValue)
let %test _= (getValeur (main "../../exemples/exemple-19.mml") = IntegerValue 4)
let%test _ = ( getValeur (main "../../exemples/exemple-20.mml") = (NullValue) )

let%test _ = ( getValeur (main "../../exemples/exemple-21.mml") = (NullValue))
let%test _ = ( getValeur (main "../../exemples/exemple-22.mml") = (BooleanValue false))
let%test _ = ( let _valeur = getValeur (main "../../exemples/exemple-23.mml") in (print_endline(string_of_value _valeur)); _valeur = (ReferenceValue "ref@1"))
let%test _ = ( getValeur (main "../../exemples/exemple-26.mml") = (IntegerValue 3))



(************************Test effet de bord (Mem) **************************)

let %test _ = pop( getMem (main "../../exemples/exemple-13.mml")) = ("ref@1", IntegerValue 11)
let %test _ = pop(getMem (main "../../exemples/exemple-14.mml")) = ("ref@1", IntegerValue 2)
let %test _ = pop(getMem (main "../../exemples/exemple-15.mml")) = ("ref@1", IntegerValue 0)
let %test _ = ( getMem (main "../../exemples/exemple-16.mml") = [("ref@1", IntegerValue 2  );("ref@2",IntegerValue 2);("ref@1", IntegerValue 0) ])
let %test _= ( getMem (main "../../exemples/exemple-17.mml") =  [("ref@1", IntegerValue 2 );("ref@2", IntegerValue 2);("ref@1", IntegerValue 0); ])

(*************************Test de Type ***************************************)
let%test _ = ( getType (main "../../exemples/exemple-00.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-01.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-02.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-03.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-04.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-05.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-06.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-07.mml") = (IntegerType) )
let%test _ = ( getType (main "../../exemples/exemple-08.mml") = (IntegerType) )
let%test _ = ( let _type = getType (main "../../exemples/exemple-09.mml") in (print_endline (string_of_type _type)); _type = (FunctionType(VariableType(ref UnknownType,1),VariableType(ref UnknownType,1))))
let%test _ = ( getType (main "../../exemples/exemple-12.mml") = (IntegerType) )
let%test _ = ( let _type = getType (main "../../exemples/exemple-200.mml") in (print_endline (string_of_type _type)); _type = ((IntegerType)))
let%test _ = ( let _type = getType (main "../../exemples/exemple-13.mml") in (print_endline (string_of_type _type)); _type = (UnitType))
let %test _ = ( getType (main "../../exemples/exemple-14.mml") = UnitType)
let %test _ = ( getType (main "../../exemples/exemple-15.mml") = IntegerType)
let %test _ = ( getType (main "../../exemples/exemple-16.mml") = UnitType)
let %test _= ( getType (main "../../exemples/exemple-17.mml") = IntegerType)
let %test _= ( getType (main "../../exemples/exemple-18.mml") = UnitType)
let %test _= (getType (main "../../exemples/exemple-19.mml") = IntegerType)
let%test _ = ( getType (main "../../exemples/exemple-20.mml") = (UnitType))
let%test _ = ( getType (main "../../exemples/exemple-21.mml") = (UnitType))
let%test _ = ( getType (main "../../exemples/exemple-22.mml") = (BooleanType))
let%test _ = ( getType (main "../../exemples/exemple-23.mml") = (ReferenceType(IntegerType)))
let%test _ = ( getType (main "../../exemples/exemple-24.mml") = (FunctionType(IntegerType,IntegerType)))
let%test _ = ( getType (main "../../exemples/exemple-25.mml") = (ErrorType))
let%test _ = ( getType (main "../../exemples/exemple-26.mml") = (IntegerType))
let%test _ = ( getType (main "../../exemples/exemple-27.mml") = (ErrorType))
let%test _ = ( getType (main "../../exemples/exemple-28.mml") = (ErrorType))
let%test _ = ( getType (main "../../exemples/exemple-29.mml") = (ErrorType))
let%test _ = ( getType (main "../../exemples/exemple-30.mml") = (FunctionType(IntegerType, BooleanType)))
let%test _ = ( getType (main "../../exemples/exemple-31.mml") = (UnitType))
let%test _ = ( getType (main "../../exemples/exemple-32.mml") = (ErrorType))