(* cabsvisit.mli *)
(* interface for cabsvisit.ml *)

(* basic interface for a visitor object *)
(* Each method is given an entity to work on, and the returned thing then *)
(* replaces the original in the tree. *)
(* All visit methods are called in postorder. *)
class type cabsVisitor = object
  method vexpr : Cabs.expression -> Cabs.expression         (* expressions *)
  method vvname : string -> string                (* variable names *)
  method vspec : Cabs.specifier -> Cabs.specifier           (* specifier *)

  (* I'll add others as I find need for them *)
end

val visitCabsDefn : Cabs.definition -> cabsVisitor -> Cabs.definition

val visitCabsExpr : Cabs.expression -> cabsVisitor -> Cabs.expression
