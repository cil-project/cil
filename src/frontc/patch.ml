(* patch.ml *)
(* CABS file patching *)

open Cabs
open Trace
open Pretty
open Cabsvisit

(* binding of a unification variable to a syntactic construct *)
type binding =
  | BSpecifier of string * spec_elem list
  | BName of string * string
  | BExpr of string * expression

(* thrown when unification fails *)
exception NoMatch

(* thrown when an attempt to find the associated binding fails *)
exception BadBind of string

(* trying to isolate performance problems *)
let verbose : bool = false


(* raise NoMatch if x and y are not equal *)
let mustEq (x : 'a) (y : 'a) : unit =
begin
  if (x <> y) then (
    if verbose then
      (trace "patchMismatch" (dprintf "mismatch by structural disequality\n"));
    raise NoMatch
  )
end


let isPatternVar (s : string) : bool =
  ((String.length s) >= 1) && ((String.get s 0) = '@')

(* 's' is actually "@name(blah)"; extract the 'blah' *)
let extractPatternVar (s : string) : string =
  (*(trace "patch" (dprintf "extractPatternVar %s\n" s));*)
  (String.sub s 6 ((String.length s) - 7))


(* class to describe how to modify the tree for subtitution *)
class substitutor (bindings : binding list) = object(self)
  (* look in the binding list for a given name *)
  method findBinding (name : string) : binding =
  begin
    try
      (List.find
        (fun b ->
          match b with
          | BSpecifier(n, _) -> n=name
          | BName(n, _) -> n=name
          | BExpr(n, _) -> n=name)
        bindings)
    with
      Not_found -> raise (BadBind ("name not found: " ^ name))
  end

  method vexpr (e:expression) : expression =
  begin
    match e with
    | EXPR_PATTERN(name) -> (
        match (self#findBinding name) with
        | BExpr(_, expr) -> expr    (* substitute bound expression *)
        | _ -> raise (BadBind ("wrong type: " ^ name))
      )
    | _ -> e
  end

  method vvname (s:string) : string =
  begin
    if (isPatternVar s) then (
      let name = (extractPatternVar s) in
      match (self#findBinding name) with
      | BName(_, str) -> str        (* substitute *)
      | _ -> raise (BadBind ("wrong type: " ^ name))
    )
    else
      s
  end

  method vspec (s:specifier) : specifier =
  begin
    match s with
    | SpecPattern(name) :: rest -> (
        match (self#findBinding name) with
        | BSpecifier(_, speclist) -> speclist @ rest
        | _ -> raise (BadBind ("wrong type: " ^ name))
      )
    | _ -> s
  end
end


(* a few debugging printers.. *)
let printExpr (e : expression) =
begin
  if (verbose && traceActive "patchPrint") then (
    Cprint.print_expression e 1; Cprint.force_new_line ();
    Cprint.flush ()
  )
end

let printSpecs (pat : spec_elem list) (tgt : spec_elem list) =
begin
  if (verbose && traceActive "patchPrint") then (
    Cprint.print_specifiers pat;  Cprint.force_new_line ();
    Cprint.print_specifiers tgt;  Cprint.force_new_line ();
    Cprint.flush ()
  )
end

let printDecl (pat : name) (tgt : name) =
begin
  if (verbose && traceActive "patchPrint") then (
    Cprint.print_name pat;  Cprint.force_new_line ();
    Cprint.print_name tgt;  Cprint.force_new_line ();
    Cprint.flush ()
  )
end

let printDefn (d : definition) =
begin
  if (verbose && traceActive "patchPrint") then (
    Cprint.print_def d;
    Cprint.flush ()
  )
end



(* why can't I have forward declarations in the language?!! *)
let unifyExprFwd : (expression -> expression -> binding list) ref
  = ref (fun e e -> [])


(* substitution for expressions *)
let substExpr (bindings : binding list) (expr : expression) : expression =
begin            
  if verbose then
    (trace "patchUnify" (dprintf "substExpr with %d bindings\n" (List.length bindings)));
  (printExpr expr);

  (* apply the transformation *)
  let result = (visitCabsExpr expr (new substitutor bindings :> cabsVisitor)) in
  (printExpr result);

  result
end

let d_loc (_:unit) (loc: cabsloc) : doc =
  text loc.filename ++ chr ':' ++ num loc.lineno


(* class to describe how to modify the tree when looking for places *)
(* to apply expression transformers *)
class exprTransformer (srcpattern : expression) (destpattern : expression)
                      (patchline : int) (srcloc : cabsloc) = object(self)
  method vexpr (e:expression) : expression =
  begin
    (* see if the source pattern matches this subexpression *)
    try (
      let bindings = (!unifyExprFwd srcpattern e) in

      (* match! *)
      (trace "patch" (dprintf "expr match: patch line %d, src %a\n"
                              patchline d_loc srcloc));
      (substExpr bindings destpattern)
    )

    with NoMatch -> (
      (* doesn't apply *)
      e
    )
  end

  (* other constructs left unchanged *)
  method vvname (s:string) = s
  method vspec (s:specifier) = s
end


let gettime () : float =
  (Unix.times ()).Unix.tms_utime

let rec applyPatch (patch : file) (src : file) : file =
begin
  (trace "patchTime" (dprintf "applyPatch start: %f\n" (gettime ())));
  if (traceActive "patchPrint") then
    Cprint.out := stdout      (* hack *)
  else ();

  (* more hackery *)
  unifyExprFwd := unifyExpr;

  (* patch a single source definition, yield transformed *)
  let rec patchDefn (patch : file) (d : definition) : definition list =
  begin
    match patch with
    | TRANSFORMER(srcpattern, destpattern, loc) :: rest -> (
        if verbose then
          (trace "patchTry"
            (dprintf "considering applying defn pattern at line %d to src at %a\n"
                     loc.lineno d_loc (get_definitionloc d)));

        (* see if the source pattern matches the definition 'd' we have *)
        try (
          let bindings = (unifyDefn srcpattern d) in

          (* we have a match!  apply the substitutions *)
          (trace "patch" (dprintf "defn match: patch line %d, src %a\n"
                                  loc.lineno d_loc (get_definitionloc d)));
          (List.map (fun destElt -> (substDefn bindings destElt)) destpattern)
        )

        with NoMatch -> (
          (* no match, continue down list *)
          (*(trace "patch" (dprintf "no match\n"));*)
          (patchDefn rest d)
        )
      )

    | EXPRTRANSFORMER(srcpattern, destpattern, loc) :: rest -> (
        if verbose then
          (trace "patchTry"
            (dprintf "considering applying expr pattern at line %d to src at %a\n"
                     loc.lineno d_loc (get_definitionloc d)));

        (* walk around in 'd' looking for expressions to modify *)
        let d' = (visitCabsDefn d (new exprTransformer srcpattern destpattern
                                                       loc.lineno (get_definitionloc d)))
        in

        (* recursively invoke myself to try additional patches *)
        (patchDefn rest d')
      )

    | _ :: rest -> (
        (* not a transformer; just keep going *)
        (patchDefn rest d)
      )
    | [] -> (
        (* reached the end of the patch file with no match *)
        [d]     (* have to wrap it in a list ... *)
      )
  end in

  (* transform all the definitions *)
  let result = (List.flatten (List.map (fun d -> (patchDefn patch d)) src)) in

  (*Cprint.print_defs result;*)

  if (traceActive "patchPrint") then (
    (* avoid flush bug? yes *)
    Cprint.force_new_line ();
    Cprint.flush ()
  );

  (trace "patchTime" (dprintf "applyPatch finish: %f\n" (gettime ())));
  result
end


(* given a definition pattern 'pat', and a target concrete defintion 'tgt', *)
(* determine if they can be unified; if so, return the list of bindings of *)
(* unification variables in pat; otherwise raise NoMatch *)
and unifyDefn (pat : definition) (tgt : definition) : binding list =
begin
  match pat, tgt with
  | DECDEF((pspecifiers, pdeclarators), _),
    DECDEF((tspecifiers, tdeclarators), _) -> (
      if verbose then
        (trace "patchUnify" (dprintf "unifyDefn of DECDEFs\n"));
      (unifySpecifiers pspecifiers tspecifiers) @
      (unifyInitDeclarators pdeclarators tdeclarators)
    )

  | TYPEDEF((pspec, pdecl), _),
    TYPEDEF((tspec, tdecl), _) -> (
      if verbose then
        (trace "patchUnify" (dprintf "unifyDefn of TYPEDEFs\n"));
      (unifySpecifiers pspec tspec) @
      (unifyDeclarators pdecl tdecl)
    )

  | ONLYTYPEDEF(pspec, _),
    ONLYTYPEDEF(tspec, _) -> (
      if verbose then
        (trace "patchUnify" (dprintf "unifyDefn of ONLYTYPEDEFs\n"));
      (unifySpecifiers pspec tspec)
    )

  | _, _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching definitions\n"));
      raise NoMatch
    )
end

and unifySpecifiers (pat : spec_elem list) (tgt : spec_elem list) : binding list =
begin
  if verbose then
    (trace "patchUnify" (dprintf "unifySpecifiers\n"));
  (printSpecs pat tgt);

  (* canonicalize the specifiers by sorting them *)
  let pat' = (List.stable_sort compare pat) in
  let tgt' = (List.stable_sort compare tgt) in

  (* if they are equal, they match with no further checking *)
  if (pat' = tgt') then [] else

  (* walk down the (unsorted..) lists *)
  let rec loop pat tgt =
    match pat, tgt with
    | [], [] -> []
    | (pspec :: prest), (tspec :: trest) when pspec = tspec ->
         (loop prest trest)
    | [SpecPattern(name)], _ ->
        (* record that future occurrances of @specifier(name) will yield this specifier *)
        if verbose then
          (trace "patchUnify" (dprintf "found specifier match for %s\n" name));
        [BSpecifier(name, tgt)]
    | _,_ -> (
        (* no match *)
        if verbose then
          (trace "patchMismatch" (dprintf "mismatching specifiers\n"));
        raise NoMatch
     )
  in
  (loop pat tgt)
end

and unifyInitDeclarators (pat : init_name list) (tgt : init_name list) : binding list =
begin
  (*
    if verbose then
      (trace "patchUnify" (dprintf "unifyInitDeclarators, pat %d, tgt %d\n"
                                   (List.length pat) (List.length tgt)));
  *)

  match pat, tgt with
  | ((pdecl, piexpr) :: prest),
    ((tdecl, tiexpr) :: trest) ->
      (unifyDeclarator pdecl tdecl) @
      (unifyInitExpr piexpr tiexpr) @
      (unifyInitDeclarators prest trest)
  | [], [] -> []
  | _, _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching init declarators\n"));
      raise NoMatch
    )
end

and unifyDeclarators (pat : name list) (tgt : name list) : binding list =
begin
  if verbose then
    (trace "patchUnify" (dprintf "unifyDeclarators (pat len %d, tgt len %d\n"
                                 (List.length pat) (List.length tgt)));

  match pat, tgt with
  | (pdecl :: prest),
    (tdecl :: trest) ->
      (unifyDeclarator pdecl tdecl) @
      (unifyDeclarators prest trest)

  | [], [] -> []
  | _, _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching declarators\n"));
      raise NoMatch
    )
end

and unifyDeclarator (pat : name) (tgt : name) : binding list =
begin
  if verbose then
    (trace "patchUnify" (dprintf "unifyDeclarator\n"));
  (printDecl pat tgt);

  match pat, tgt with
  | (pname, pdtype, pattr),
    (tname, tdtype, tattr) ->
      (mustEq pattr tattr);
      (unifyDeclType pdtype tdtype) @
      (unifyName pname tname)
end

and unifyDeclType (pat : decl_type) (tgt : decl_type) : binding list =
begin
  if verbose then
    (trace "patchUnify" (dprintf "unifyDeclType\n"));

  match pat, tgt with
  | JUSTBASE, JUSTBASE -> []
  | PARENTYPE(pattr1, ptype, pattr2),
    PARENTYPE(tattr1, ttype, tattr2) ->
      (mustEq pattr1 tattr1);
      (mustEq pattr2 tattr2);
      (unifyDeclType ptype ttype)
  | ARRAY(ptype, psz),
    ARRAY(ttype, tsz) ->
      (unifyDeclType ptype ttype) @
      (unifyExpr psz tsz)
  | PTR(pattr, ptype),
    PTR(tattr, ttype) ->
      (mustEq pattr tattr);
      (unifyDeclType ptype ttype)
  | PROTO(ptype, pformals, pva),
    PROTO(ttype, tformals, tva) ->
      (mustEq pva tva);
      (unifyDeclType ptype ttype) @
      (unifySingleNames pformals tformals)
  | _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching decl_types\n"));
      raise NoMatch
    )
end

and unifySingleNames (pat : single_name list) (tgt : single_name list) : binding list =
begin
  if verbose then
    (trace "patchUnify" (dprintf "unifySingleNames, pat %d, tgt %d\n"
                                 (List.length pat) (List.length tgt)));

  match pat, tgt with
  | [], [] -> []
  | (pspec, pdecl) :: prest,
    (tspec, tdecl) :: trest ->
      (unifySpecifiers pspec tspec) @
      (unifyDeclarator pdecl tdecl) @
      (unifySingleNames prest trest)
  | _, _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching single_name lists\n"));
      raise NoMatch
    )
end

and unifyName (pat : string) (tgt : string) : binding list =
begin
  (* equal? match with no further ado *)
  if (pat = tgt) then [] else

  (* is the pattern a variable? *)
  if (isPatternVar pat) then
    (* pat is actually "@name(blah)"; extract the 'blah' *)
    let varname = (extractPatternVar pat) in

    (* when substituted, this name becomes 'tgt' *)
    if verbose then
      (trace "patchUnify" (dprintf "found name match for %s\n" varname));
    [BName(varname, tgt)]

  else (
    if verbose then
      (trace "patchMismatch" (dprintf "mismatching names: %s and %s\n" pat tgt));
    raise NoMatch
  )
end

and unifyExpr (pat : expression) (tgt : expression) : binding list =
begin
  (* if they're equal, that's good enough *)
  if (pat = tgt) then [] else

  (* shorter name *)
  let ue = unifyExpr in

  (* because of the equality check above, I can omit some cases *)
  match pat, tgt with
  | UNARY(pop, pexpr),
    UNARY(top, texpr) ->
      (mustEq pop top);
      (ue pexpr texpr)
  | BINARY(pop, pexp1, pexp2),
    BINARY(top, texp1, texp2) ->
      (mustEq pop top);
      (ue pexp1 texp1) @
      (ue pexp2 texp2)
  | QUESTION(p1, p2, p3),
    QUESTION(t1, t2, t3) ->
      (ue p1 t1) @
      (ue p2 t2) @
      (ue p3 t3)
  | CAST((pspec, ptype), piexpr),
    CAST((tspec, ttype), tiexpr) ->
      (mustEq ptype ttype);
      (unifySpecifiers pspec tspec) @
      (unifyInitExpr piexpr tiexpr)
  | CALL(pfunc, pargs),
    CALL(tfunc, targs) ->
      (ue pfunc tfunc) @
      (unifyExprs pargs targs)
  | COMMA(pexprs),
    COMMA(texprs) ->
      (unifyExprs pexprs texprs)
  | EXPR_SIZEOF(pexpr),
    EXPR_SIZEOF(texpr) ->
      (ue pexpr texpr)
  | TYPE_SIZEOF(pspec, ptype),
    TYPE_SIZEOF(tspec, ttype) ->
      (mustEq ptype ttype);
      (unifySpecifiers pspec tspec)
  | EXPR_ALIGNOF(pexpr),
    EXPR_ALIGNOF(texpr) ->
      (ue pexpr texpr)
  | TYPE_ALIGNOF(pspec, ptype),
    TYPE_ALIGNOF(tspec, ttype) ->
      (mustEq ptype ttype);
      (unifySpecifiers pspec tspec)
  | INDEX(parr, pindex),
    INDEX(tarr, tindex) ->
      (ue parr tarr) @
      (ue pindex tindex)
  | MEMBEROF(pexpr, pfield),
    MEMBEROF(texpr, tfield) ->
      (mustEq pfield tfield);
      (ue pexpr texpr)
  | MEMBEROFPTR(pexpr, pfield),
    MEMBEROFPTR(texpr, tfield) ->
      (mustEq pfield tfield);
      (ue pexpr texpr)
  | GNU_BODY(pblock),
    GNU_BODY(tblock) ->
      (mustEq pblock tblock);
      []
  | EXPR_PATTERN(name), _ ->
      (* match, and contribute binding *)
      if verbose then
        (trace "patchUnify" (dprintf "found expr match for %s\n" name));
      [BExpr(name, tgt)]
  | a, b ->
      if (verbose && traceActive "patchMismatch") then (
        (trace "patchMismatch" (dprintf "mismatching expression\n"));
        (printExpr a);
        (printExpr b)
      );
      raise NoMatch
end

and unifyInitExpr (pat : init_expression) (tgt : init_expression) : binding list =
begin
  (*
    Cprint.print_init_expression pat;  Cprint.force_new_line ();
    Cprint.print_init_expression tgt;  Cprint.force_new_line ();
    Cprint.flush ();
  *)

  match pat, tgt with
  | NO_INIT, NO_INIT -> []
  | SINGLE_INIT(pe), SINGLE_INIT(te) ->
      (unifyExpr pe te)
  | COMPOUND_INIT(plist),
    COMPOUND_INIT(tlist) -> (
      let rec loop plist tlist =
        match plist, tlist with
        | ((pwhat, piexpr) :: prest),
          ((twhat, tiexpr) :: trest) ->
            (mustEq pwhat twhat);
            (unifyInitExpr piexpr tiexpr) @
            (loop prest trest)
        | [], [] -> []
        | _, _ -> (
            if verbose then
              (trace "patchMismatch" (dprintf "mismatching compound init exprs\n"));
            raise NoMatch
          )
      in
      (loop plist tlist)
    )
  | _,_ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching init exprs\n"));
      raise NoMatch
    )
end

and unifyExprs (pat : expression list) (tgt : expression list) : binding list =
begin
  match pat, tgt with
  | (pexpr :: prest), (texpr :: trest) ->
      (unifyExpr pexpr texpr) @
      (unifyExprs prest trest)
  | [], [] -> []
  | _, _ -> (
      if verbose then
        (trace "patchMismatch" (dprintf "mismatching expression lists\n"));
      raise NoMatch
    )
end

(* given the list of bindings 'b', substitute them into 'd' to yield a new definition *)
and substDefn (bindings : binding list) (defn : definition) : definition =
begin
  if verbose then
    (trace "patchUnify" (dprintf "substDefn with %d bindings\n" (List.length bindings)));
  (printDefn defn);

  (* apply the transformation *)
  (visitCabsDefn defn (new substitutor bindings :> cabsVisitor))
end


(* end of file *)
