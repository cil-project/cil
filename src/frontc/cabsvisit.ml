(* cabsvisit.ml *)
(* tree visitor and rewriter for cabs *)

open Cabs
open Trace
open Pretty
module E = Errormsg

(* basic interface for a visitor object *)
(* Each method is given an entity to work on, and the returned thing then *)
(* replaces the original in the tree. *)
(* All visit methods are called in postorder. *)
class type cabsVisitor = object
  method vexpr : expression -> expression         (* expressions *)
  method vvname : string -> string                (* variable names *)
  method vspec : specifier -> specifier           (* specifier *)

  (* I'll add others as I find need for them *)
end

(* a default visitor which does nothing to the tree *)
class nopCabsVisitor = object
  method vexpr (e:expression) = e
  method vvname (s:string) = s
  method vspec (s:specifier) = s
end


(* currently the only entry point is at the definition level *)
let visitCabsDefn (src : definition) (vis : cabsVisitor) : definition =
begin
  let rec vtypeSpec (tspec : typeSpecifier) : typeSpecifier =
  begin
    match tspec with
    | Tstruct(sname, Some(fields)) -> Tstruct(sname, Some(vfields fields))
    | Tunion(uname, Some(fields)) -> Tunion(uname, Some(vfields fields))
    | TtypeofE(expr) -> TtypeofE(vexpr expr)
    | TtypeofT(spec, decltype) -> TtypeofT(vspec spec, vdecltype decltype)
    | _ -> tspec          (* no recursive structure *)
  end

  (* nothing for 'storage' *)

  and vspec (s : specifier) : specifier =
  begin
    let s' = 
      match s with
      | elem :: rest -> (
          let e =
            match elem with
            | SpecType(tspec) -> SpecType(vtypeSpec tspec)
            | _ -> elem
          in
          (vis#vspec (e :: vspec rest))
        )
      | [] -> []
    in

    (vis#vspec s')     (* call user's function *)
  end

  and vdecltype (d : decl_type) : decl_type =
  begin
    match d with
    | PARENTYPE(attrs1, decltype, attrs2) ->
        PARENTYPE(vattrlist attrs1, vdecltype decltype, vattrlist attrs2)
    | ARRAY(decltype, e) ->
        ARRAY(vdecltype decltype, vexpr e)
    | PTR(attrs, decltype) ->
        PTR(vattrlist attrs, vdecltype decltype)
    | PROTO(decltype, args, isva) ->
        PROTO(vdecltype decltype, vsinglenames args, isva)
    | _ -> d
  end

  (* name_group always unrolled where found *)

  and vdecllist (d : name list) : name list =
  begin
    match d with
    | [] -> []
    | decl :: rest -> (vdeclarator decl) :: (vdecllist rest)
  end

  and vfields (f : field_group list) : field_group list =
  begin
    match f with
    | [] -> []
    | (spec, decllist) :: rest -> (
        (vspec spec,
         List.map (fun elt ->
                     match elt with
                     | (fielddecl, Some(expr)) ->
                         (vdeclarator fielddecl, Some(vexpr expr))
                     | (fielddecl, None) ->
                         (vdeclarator fielddecl, None)
                  ) decllist) ::
        (vfields rest)
      )
  end

  (* init_name_group is always unrolled *)

  and vdeclarator (d : name) : name =
  begin
    match d with
    | (varname, decltype, attrs) ->
        (vis#vvname varname, vdecltype decltype, vattrlist attrs)
  end

  (* init_name is always unrolled *)

  and vinitdecllist (decllist : init_name list) : init_name list =
  begin
    match decllist with
    | [] -> []
    | (name, iexpr) :: rest ->
        (vdeclarator name, vinitexpr iexpr) :: vinitdecllist rest
  end

  (* single_name is always unrolled *)

  and vsinglenames (sn : single_name list) : single_name list =
  begin
    match sn with
    | [] -> []
    | (spec, decl) :: rest ->
        (vspec spec, vdeclarator decl) :: (vsinglenames rest)
  end

  (* enum_item is always unrolled *)

  and vdefn (d : definition) : definition =
  begin
    match d with
    | FUNDEF((fspec, fdecl), b, l) -> 
        FUNDEF((vspec fspec, vdeclarator fdecl), vblock b, l)
    | DECDEF((spec, decl), l) -> DECDEF((vspec spec, vinitdecllist decl), l)
    | TYPEDEF((spec, decl), l) -> TYPEDEF((vspec spec, vdecllist decl), l)
    | PRAGMA(e, l) -> PRAGMA(vexpr e, l)
    | TRANSFORMER(src, destlist, l) ->
        TRANSFORMER(vdefn src, List.map vdefn destlist, l)
    | EXPRTRANSFORMER(src, dest, l) ->
        EXPRTRANSFORMER(vexpr src, vexpr dest, l)
    | _ -> d
  end

  and vblock blk : block =
  begin
    { blabels = blk.blabels;
      battrs = vattrlist blk.battrs;
      bdefs = List.map vdefn blk.bdefs;
      bstmts = List.map vstmt blk.bstmts;
    } 
  end

  and vstmt (s : statement) : statement =
  begin
    match s with
    | COMPUTATION(e, l) -> COMPUTATION(vexpr e, l)
    | BLOCK(b, l) -> BLOCK(vblock b, l)
    | SEQUENCE(s1, s2, l) -> SEQUENCE(vstmt s1, vstmt s2, l)
    | IF(e, s1, s2, l) -> IF(vexpr e, vstmt s1, vstmt s2, l)
    | WHILE(e, s, l) -> WHILE(vexpr e, vstmt s, l)
    | DOWHILE(e, s, l) -> DOWHILE(vexpr e, vstmt s, l)
    | FOR(e1, e2, e3, s, l) -> FOR(vexpr e1, vexpr e2, vexpr e3, vstmt s, l)
    | RETURN(e, l) -> RETURN(vexpr e, l)
    | SWITCH(e, s, l) -> SWITCH(vexpr e, vstmt s, l)
    | CASE(e, s, l) -> CASE(vexpr e, vstmt s, l)
    | CASERANGE(e1, e2, s, l) -> CASERANGE(vexpr e1, vexpr e2, vstmt s, l)
    | DEFAULT(s, l) -> DEFAULT(vstmt s, l)
    | LABEL(str, s, l) -> LABEL(str, vstmt s, l)
    | COMPGOTO(e, l) -> COMPGOTO(vexpr e, l)
    | ASM(templ, voltl, constraints, outputs, inputs, loc) ->
        ASM(templ,
            voltl,
            List.map (fun (s,e) -> (s, vexpr e)) constraints,
            List.map (fun (s,e) -> (s, vexpr e)) outputs,
            inputs,
            loc)
    | _ -> s        (* doesn't contain expressions *)
  end

  and vexpr (e : expression) : expression =
  begin
    if (traceActive "visitCabsExpr") then (
      (trace "visitCabsExpr" (dprintf "visiting expression:\n"));
      Cprint.print_expression e 1; Cprint.force_new_line (); Cprint.flush ();
    );

    let transformed = 
      match e with
      | UNARY(o, e) -> UNARY(o, vexpr e)
      | BINARY(o, e1, e2) -> BINARY(o, vexpr e1, vexpr e2)
      | QUESTION(e1, e2, e3) -> QUESTION(vexpr e1, vexpr e2, vexpr e3)
      | CAST((spec, decl), iexpr) -> CAST((spec, vdecltype decl), vinitexpr iexpr)
      | CALL(f, args) -> CALL(vexpr f, (List.map vexpr args))
      | COMMA(exprs) -> COMMA(List.map vexpr exprs)
      | EXPR_SIZEOF(e) -> EXPR_SIZEOF(vexpr e)
      | EXPR_ALIGNOF(e) -> EXPR_ALIGNOF(vexpr e)
      | INDEX(a, i) -> INDEX(vexpr a, vexpr i)
      | MEMBEROF(e, field) -> MEMBEROF(vexpr e, field)
      | MEMBEROFPTR(e, field) -> MEMBEROFPTR(vexpr e, field)
      | GNU_BODY(b) -> GNU_BODY(vblock b)
      | _ -> e        (* no subexpressions *)
    in

    (* call the user's function *)
    (vis#vexpr transformed)
  end

  and vinitexpr (iexpr : init_expression) : init_expression =
  begin
    match iexpr with
    | NO_INIT -> iexpr
    | SINGLE_INIT(expr) -> SINGLE_INIT(vexpr expr)
    | COMPOUND_INIT(cinitlist) -> COMPOUND_INIT(
        List.map (fun (what, iexpr) -> (vinitwhat what, vinitexpr iexpr)) cinitlist
      )
  end

  and vinitwhat (what : initwhat) : initwhat =
  begin
    match what with
    | NEXT_INIT -> NEXT_INIT
    | INFIELD_INIT(field, w) -> INFIELD_INIT(field, vinitwhat w)
    | ATINDEX_INIT(expr, w) -> ATINDEX_INIT(vexpr expr, vinitwhat w)
    | ATINDEXRANGE_INIT(e1, e2) -> ATINDEXRANGE_INIT(vexpr e1, vexpr e2)
  end

  and vattrlist (attrs : attribute list) : attribute list =
  begin
    match attrs with
    | [] -> []
    | (str, exprs) :: rest ->
        (str, List.map vexpr exprs) :: (vattrlist rest)
  end
  
  in
  (vdefn src)
end


(* I need an entry point for expressions too *)
let visitCabsExpr (src : expression) (vis : cabsVisitor) : expression =
begin
  (* major hack: wrap it in a pragma so I can use my definition entry point *)
  let prgma = PRAGMA(src, { lineno=0; filename=""; }) in
  match (visitCabsDefn prgma vis) with
  | PRAGMA(retval, _) -> retval
  | _ -> E.s (E.bug "what the?")
end


(* end of file *)
