(** Code to compute the control-flow graph of a function or file.  
  This will fill in the [preds] and [succs] fields of {!Cil.stmt}

  This is required for several other extensions, such as {!Dataflow}. 
*)

(** Compute the CFG for an entire file, by calling cfgFun on each function. *)
val computeFileCFG: Cil.file -> unit

(** clear the sid, succs, and preds fields of each statement. *)
val clearFileCFG: Cil.file -> unit

(** Compute a control flow graph for fd.  Stmts in fd have preds and succs
  filled in *)
val cfgFun : Cil.fundec -> int

(** clear the sid, succs, and preds fields of each statment in a function *)
val clearCFGinfo: Cil.fundec -> unit

(** print control flow graph (in dot form) for fundec to channel *)
val printCfgChannel : out_channel -> Cil.fundec -> unit

(** Print control flow graph (in dot form) for fundec to file *)
val printCfgFilename : string -> Cil.fundec -> unit

(** Next statement id that will be assigned. *)
val start_id: int ref

(** Return all statements in a file - valid after computeFileCfg only *)
val allStmts : Cil.file -> Cil.stmt list
