
open Cil
open Pretty

(** compute use/def information *)

module VS = Set.Make (struct 
                        type t = Cil.varinfo
                        let compare v1 v2 = Pervasives.compare v1.vid v2.vid
                      end)

(** Set this global to how you want to handle function calls *)
let getUseDefFunctionRef: (exp -> VS.t * VS.t) ref = 
  ref (fun _ -> (VS.empty, VS.empty))

(** Say if you want to consider a variable use *)
let considerVariableUse: (varinfo -> bool) ref = 
  ref (fun _ -> true)


(** Say if you want to consider a variable def *)
let considerVariableDef: (varinfo -> bool) ref = 
  ref (fun _ -> true)

(** Save if you want to consider a variable addrof as a use *)
let considerVariableAddrOfAsUse: (varinfo -> bool) ref = 
  ref (fun _ -> true)

let varUsed: VS.t ref = ref VS.empty
let varDefs: VS.t ref = ref VS.empty

class useDefVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor
      
  (** this will be invoked on variable definitions only because we intercept 
   * all uses of variables in expressions ! *)
  method vvrbl (v: varinfo) = 
    if (!considerVariableDef) v then 
      varDefs := VS.add v !varDefs;
    SkipChildren

  method vexpr = function
      Lval (Var v, off) -> 
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableUse) v then
          varUsed := VS.add v !varUsed;
        SkipChildren (* So that we do not see the v *)

    | AddrOf (Var v, off) 
    | StartOf (Var v, off) -> 
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableAddrOfAsUse) v then 
          varUsed := VS.add v !varUsed;
        SkipChildren

    | _ -> DoChildren

  (* For function calls, do the transitive variable read/defs *)
  method vinst = function
      Call (_, f, _, _) -> begin
        (* we will call DoChildren to compute the use and def that appear in 
         * this instruction. We also add in the stuff computed by 
         * getUseDefFunctionRef *)
        let use, def = !getUseDefFunctionRef f in
        varUsed := VS.union !varUsed use;
        varDefs := VS.union !varDefs def;
        DoChildren;
      end
    | _ -> DoChildren
        
end

let useDefVisitor = new useDefVisitorClass 

(** Compute the use information for an expression (accumulate to an existing 
 * set) *)
let computeUseExp ?(acc=VS.empty) (e: exp) : VS.t = 
  varUsed := acc;
  ignore (visitCilExpr useDefVisitor e);
  !varUsed


(** Compute the use/def information for an instruction *)
let computeUseDefInstr ?(acc_used=VS.empty)
                       ?(acc_defs=VS.empty) 
                       (i: instr) : VS.t * VS.t = 
  varUsed := acc_used;
  varDefs := acc_defs;
  ignore (visitCilInstr useDefVisitor i);
  !varUsed, !varDefs


(** Compute the use/def information for a statement kind. Do not descend into 
 * the nested blocks. *)
let computeUseDefStmtKind ?(acc_used=VS.empty)
                          ?(acc_defs=VS.empty) 
                          (sk: stmtkind) : VS.t * VS.t =
  varUsed := acc_used;
  varDefs := acc_defs;
  let ve e = ignore (visitCilExpr useDefVisitor e) in 
  let _ = 
    match sk with 
      Return (None, _) -> ()
    | Return (Some e, _) -> ve e
    | If (e, _, _, _) -> ve e
    | Break _ | Goto _ | Continue _ -> ()
    | Loop (_, _, _, _) -> ()
    | Switch (e, _, _, _) -> ve e
    | Instr il -> 
        List.iter (fun i -> ignore (visitCilInstr useDefVisitor i)) il
    | TryExcept _ | TryFinally _ -> ()
    | Block _ -> ()
  in
  !varUsed, !varDefs



