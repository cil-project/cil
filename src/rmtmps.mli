(* rmtmps.mli *)
(* remove unused things from cil files:               *)
(*   - local temporaries introduced but not used      *)
(*   - globla declarations that are not used          *)
(*   - types that are not used                        *)

(* process a complete Cil file *)
val removeUnusedTemps: Cil.file -> unit

