(* 
 * Given a fixed CIL file, modify it so that it behaves more like a
 * control-flow graph for the purposes of analysis.
 *
 * The CIL file will not have break, default or continue statements. 
 * The "succs" and "preds" fields for every statement should be set
 * correctly. Note that the backedge from the end of a Loop(block) to the
 * beginning will not be in that list, however.
 *
 * In addition, all statements are given unique IDs. 
 *)
val make_cfg : Cil.file -> unit
