val stackChecks : bool ref

val boxFile: Cil.file -> Cil.file

val customAttrPrint: Cil.attribute -> Pretty.doc option


(* If you set defaultIsWild then all pointer types without a qualifier are 
 * WILD, all arrays are not!!! SIZED and all variables whose address is taken 
 * or which are globals or which contain arrays are TAGGED  *)

