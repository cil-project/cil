open Cil


(** Compute a control flow graph for fd.  Stmts in fd have preds and succs filled in *)
val cfgFun : fundec -> int

(** Compute the CFG for an entire file, by calling cfgFun on each function. *)
val computeFileCFG: Cil.file -> unit

(** clear the sid, succs, and preds fields of each statement. *)
val clearFileCFG: Cil.file -> unit

(** clear the sid, succs, and preds fields of each statment in a function *)
val clearCFGinfo: Cil.fundec -> unit

(** print control flow graph (in dot form) for fundec to channel *)
val printCfgChannel : out_channel -> fundec -> unit

(** Print control flow graph (in dot form) for fundec to file *)
val printCfgFilename : string -> fundec -> unit

(** Next statement id that will be assigned. *)
val start_id: int ref

(** All of the nodes in a file. *)
val nodeList : stmt list ref
(** number of nodes in the CFG *)
val numNodes : int ref
