
(** An optimized based on symbolic evaluation *)
val optim: Cil.file -> Cil.file


(** A list of functions to be forcefully removed from the file. This only work 
 * for functions that return void *)
val forcefullyRemove: string list ref


(** Set this to true if you want all cheks removed *)
val forcefullyRemoveAll: bool ref

