(* An optimizer based on symbolic evaluation (to account for simple 
 * assignments *)
open Cil
open Pretty
open Clist

module E = Errormsg
module H = Hashtbl


(* Register file. Maps identifiers of local variables to expressions. *)
let regFile : (int, exp) H.t = H.create 29


(* Rewrite an expression based on the current register file *)
class rewriteExpClass : cilVisitor = 
   inherit nopVisitor
   
   method vexpr = function
   | Lval (Var (tmpvar, NoOffset)) -> 
   try
   H.mem 

when not tmpvar.vglob ->
          1
   | Lval (Var _, _) -> DoChildren
   | Lval (Mem _, _) -> hasMem := true; DoChildren
end




