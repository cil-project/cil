
val solver: string ref (* Set this to which solver we are using *)


(* Call this to construct the pointer graph *)
val markFile: Cil.file -> Cil.file
