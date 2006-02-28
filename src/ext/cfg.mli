open Cil


(** Compute a control flow graph for fd.  Stmts in fd have preds and succs filled in *)
val cfgFun : fundec -> int

(** print control flow graph (in dot form) for fundec to channel *)
val printCfgChannel : out_channel -> fundec -> unit

(** Print control flow graph (in dot form) for fundec to file *)
val printCfgFilename : string -> fundec -> unit
