
(* Make sure that there is only one Return statement in the whole body. 
 * Replace all the other returns with Goto. Make sure that there is a return 
 * if the function is supposed to return something. *)
val oneret: Cil.fundec -> unit
