(*
 * Main

 The main function.
*)
open Arg
open Pretty

let main () =
  if (Array.length Sys.argv < 2) then begin
    Printf.printf "usage:\ncil [somefile.c]\n" ;
    exit 1
  end else begin
    Printf.printf "Parsing %s ...\n" (Sys.argv.(1)) ;
		(* this calls an OCAML procedure that calls our external C 
                 * procedure which calls Scott's parser and then converts the 
                 * results ...  *)
    let cil_result = Cil.parse (Sys.argv.(1)) in
    printf "%a\n" Cil.d_program cil_result ; (* print it out! *)
    exit 0
  end
      ;;

Printexc.catch main ()
;;
