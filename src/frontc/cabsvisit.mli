(* cabsvisit.mli *)
(* interface for cabsvisit.ml *)

(* Different visiting actions. 'a will be instantiated with exp, instr, etc. *)
type 'a visitAction = 
    SkipChildren                        (* Do not visit the children. Return 
                                         * the node as it is *)
  | ChangeTo of 'a                      (* Replace the expression with the 
                                         * given one *)
  | DoChildren                          (* Continue with the children of this 
                                         * node. Rebuild the node on return 
                                         * if any of the children changes 
                                         * (use == test) *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (* First consider that the entire 
                                          * exp is replaced by the first 
                                          * paramenter. Then continue with 
                                          * the children. On return rebuild 
                                          * the node if any of the children 
                                          * has changed and then apply the 
                                          * function on the node *)

type nameKind = 
    NVar                                (* Variable or function name *)
  | NField                              (* The name of a field *)
  | NType                               (* The name of a type *)


(* All visit methods are called in preorder! (but you can use 
 * ChangeDoChildrenPost to change the order) *)
class type cabsVisitor = object
  method vexpr: Cabs.expression -> Cabs.expression visitAction   (* expressions *)
  method vinitexpr: Cabs.init_expression -> Cabs.init_expression visitAction   
  method vstmt: Cabs.statement -> Cabs.statement list visitAction
  method vblock: Cabs.block -> Cabs.block visitAction
  method vvar: string -> string                  (* use of a variable 
                                                        * names *)
  method vdef: Cabs.definition -> Cabs.definition list visitAction
  method vtypespec: Cabs.typeSpecifier -> Cabs.typeSpecifier visitAction
  method vdecltype: Cabs.decl_type -> Cabs.decl_type visitAction

      (* For each declaration we call vname *)
  method vname: nameKind -> Cabs.specifier -> Cabs.name -> Cabs.name visitAction
  method vspec: Cabs.specifier -> Cabs.specifier visitAction     (* specifier *)
  method vattr: Cabs.attribute -> Cabs.attribute list visitAction


  method vEnterScope: unit -> unit
  method vExitScope: unit -> unit
end


class nopCabsVisitor: cabsVisitor


val visitCabsTypeSpecifier: cabsVisitor -> 
                            Cabs.typeSpecifier -> Cabs.typeSpecifier
val visitCabsSpecifier: cabsVisitor -> Cabs.specifier -> Cabs.specifier
val visitCabsDeclType: cabsVisitor -> Cabs.decl_type -> Cabs.decl_type
val visitCabsDefinition: cabsVisitor -> Cabs.definition -> Cabs.definition list
val visitCabsBlock: cabsVisitor -> Cabs.block -> Cabs.block
val visitCabsStatement: cabsVisitor -> Cabs.statement -> Cabs.statement list
val visitCabsExpression: cabsVisitor -> Cabs.expression -> Cabs.expression
val visitCabsAttributes: cabsVisitor -> Cabs.attribute list 
                                     -> Cabs.attribute list
val visitCabsName: cabsVisitor -> nameKind 
                   -> Cabs.specifier -> Cabs.name -> Cabs.name
val visitCabsFile: cabsVisitor -> Cabs.file -> Cabs.file


