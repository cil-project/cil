(* Things may need to be changed
   ATTRIBUTE --- Raymond ADDED them back
   Struct tag --- Since it is diffcult to rename struct tag without
                  scanning ahead, I am not renaming struct tag at all
   *)
(* :: is tricky.  For (a :: b), eval b and then a *)
(* combine --- combine C source files into one.
**
** Project: FrontC
** File:  combine.ml
** Version: 1.0 (CABS To CABS)
** Date:  4.10.01
** Author:  Raymond To
**
**  1.0   4.10.01 Raymond To First version.
*)

open Cabs
module E = Errormsg
module H = Hashtbl


(* Hash tables for combiner *)

(* Tables that used to keep track of re-definitions 
 - store definitions in tables *)
let gDefTable = H.create 307 (* global definition *)
let fDefTable = H.create 107 (* static global definition (file scope) *)

let gAlphaTable : (string, string) H.t = 
  H.create 307 (* id names that have been used *)

(* Table that maps id to renamed id *)
let gMap = H.create 107 (* global id *)
let fMap = H.create 107 (* file scope id *)
let lMap = H.create 107 (* function scope id *)

let fTag = H.create 107 (* tags that have appeared in file *)
let gTag = H.create 107 (* tags that have appeared in ALL files *)

let structTag = H.create 107
let unionTag = H.create 107
let enumTag = H.create 107


let noloc = { lineno = 0; filename = "" }

let rec combineAttr (s, el) = 
  (s, List.map combine_expression el)

and combineAttrs al = 
  List.map combineAttr al

and combine_specs (specs: spec_elem list) = 
  let combine_fields flds =
      (* Compile a list of field ids *)
    let ids = 
      List.flatten 
        (List.map (fun (_, names)  -> 
          (List.map (fun (id, _, _) -> id) names)) flds) in
    begin
      List.iter (fun id -> H.add lMap id id) ids;
      let flds' = List.map (fun fld -> combine_name_group "" fld) flds in 
      List.iter (fun id -> H.remove lMap id) ids;
      flds'
    end
  in
  let combine_enums (es: enum_item list) = 
    List.map (fun (s, e) -> (s, combine_expression e)) es
  in
  let combine_spec_elem (elem: spec_elem) = 
    match elem with
      SpecTypedef | SpecInline | SpecStorage _ -> elem
    | SpecType t ->
        SpecType
          (match t with
            Tnamed n -> Tnamed (lookup_id "type" n)
          | Tstruct (n, None) -> Tstruct (lookup_tag n, None)
          | Tstruct (n, Some flds) -> 
              Tstruct (lookup_tag n, Some (combine_fields flds))
          | Tunion (n, None) -> Tunion (lookup_tag n, None)
          | Tunion (n, Some flds) -> 
              Tunion (lookup_tag n, Some (combine_fields flds))
          | Tenum (n, None) -> Tenum(lookup_tag n, None)
          | Tenum (n, Some es) -> Tenum(lookup_tag n, Some (combine_enums es))
          | TtypeofE e -> TtypeofE (combine_expression e)
          | TtypeofT (s,d) -> 
              let s', d' = combine_only_type (s, d) in
              TtypeofT (s', d')
          | ts -> ts)
    | SpecAttr attr -> SpecAttr (combineAttr attr) 
  in
  List.map combine_spec_elem specs


and combine_decl_type = function
  | JUSTBASE -> JUSTBASE
  | BITFIELD exp -> BITFIELD (combine_expression exp)
  | PROTO (typ, pars, ell) -> 
      PROTO(combine_decl_type typ, combine_params pars, ell)
  | PTR (attrs, typ) -> PTR(combineAttrs attrs, combine_decl_type typ)
  | ARRAY (typ, dim) -> ARRAY(combine_decl_type typ, combine_expression dim)
  | PARENTYPE (al1, typ, al2) -> 
      PARENTYPE (combineAttrs al1, combine_decl_type typ,
                 combineAttrs al2)



(* Look for a number to append to the name to make it unique *)
and findNewName (usedNames: (string, 'a) H.t) (tag: string) =
  let rec loop n = 
    let newtag = tag ^ "__" ^ string_of_int n in
    if H.mem usedNames newtag then
      loop (n + 1)
    else
      newtag
  in
  if H.mem usedNames tag then
    loop 0
  else
    tag

(* return empty string if tag begins with __anon
   lookup from hashtable otherwise  *)
and lookup_tag tag = begin
  if ((String.length tag) >= 6 && (String.sub tag 0 6) = "__anon") then 
    ((*prerr_endline "Found __anon tag";*) "")
  else
    try
      H.find fTag tag
    with Not_found ->
      (try
         H.find gTag tag
       with Not_found -> (* tag has not been used at all *)
         (H.add gTag tag tag;
          H.add fTag tag tag;
          tag))
  
end
        
and combine_only_type (specs, dt) =
  (combine_specs specs, combine_decl_type dt)

(* ATTRIBUTES ARE ADDED BACK *)    
and combine_name (kind: string) ((id, typ, al) : name) = begin
 if id = "___missing_field_name" then
   (id, combine_decl_type typ, combineAttrs al)
 else
   (lookup_id kind id, combine_decl_type typ, combineAttrs al)
end
        
and combine_name_group (kind: string) (specs, names) =
  (combine_specs specs, List.map (combine_name kind) names)
    
and combine_single_name (kind: string) (specs, name) =
  (combine_specs specs, combine_name kind name) 

(* Raymond added declare_id lookup_id *)
and combine_params (pars : single_name list) = begin
  List.iter (fun name -> declare_id "" name false) pars;
  List.map (fun single_name -> combine_single_name "" single_name) pars
end

        
and combine_exps exps =
  List.map combine_expression exps

and combine_init_expression (iexp: init_expression) : init_expression = 
  match iexp with
    NO_INIT -> NO_INIT
  | SINGLE_INIT e -> SINGLE_INIT (combine_expression e)
  | COMPOUND_INIT initexps ->
      let doinitexp = function
          NEXT_INIT, e -> (NEXT_INIT, combine_init_expression e)
        | i, e -> 
            let rec doinit = function
                NEXT_INIT -> NEXT_INIT
              | INFIELD_INIT (fn, i) -> INFIELD_INIT(fn, doinit i)
              | ATINDEX_INIT (e, i) -> 
                  ATINDEX_INIT(combine_expression e, doinit i)                              in
            (doinit i, combine_init_expression e)
      in
      COMPOUND_INIT (List.map doinitexp initexps)
        
(* No need to rename fields *)    
and combine_expression (exp : expression) : expression =
  match exp with
    NOTHING ->
      NOTHING
  | UNARY (op, exp') ->
      UNARY(op, combine_expression exp')  
  | BINARY (op, exp1, exp2) ->
      BINARY(op, combine_expression exp1, combine_expression exp2)      
  | QUESTION (exp1, exp2, exp3) ->
      QUESTION(combine_expression exp1, 
               combine_expression exp2, combine_expression exp3)    
  | CAST (typ, exp) ->
      CAST(combine_only_type typ, combine_expression exp)     
  | CALL (exp, args) ->
      CALL(combine_expression exp, combine_exps args)   
  | COMMA exps ->
      COMMA(combine_exps exps)
  | CONSTANT cst ->
      CONSTANT(
        (match cst with
        CONST_INT i -> CONST_INT i
      | CONST_FLOAT r -> CONST_FLOAT r
      | CONST_CHAR c -> CONST_CHAR c
      | CONST_STRING s -> CONST_STRING s))

  | VARIABLE name -> 
      VARIABLE(lookup_id "" name)
  | EXPR_SIZEOF exp ->
      EXPR_SIZEOF (combine_expression exp)
  | TYPE_SIZEOF (specs, dt) ->
      TYPE_SIZEOF(combine_specs specs, combine_decl_type dt)
  | INDEX (exp, idx) ->
      INDEX(combine_expression exp, combine_expression idx)
  | MEMBEROF (exp, fld) ->
      MEMBEROF(combine_expression exp, fld)
  | MEMBEROFPTR (exp, fld) ->
      MEMBEROFPTR(combine_expression exp, fld)
  | GNU_BODY blk ->
      GNU_BODY (List.map combineBlkElem blk)

and combineBlkElem = function
      BDEF d -> BDEF(combine_def d false)
    | BSTM s -> BSTM(combine_statement s)

(*
** Statement combining
*)
and combine_statement stat =
  match stat with
    NOP loc->
      NOP loc
  | COMPUTATION (exp, loc) ->
      COMPUTATION(combine_expression exp, loc)
  | BLOCK (blk, loc) ->
      BLOCK(List.map combineBlkElem blk, loc)
  | SEQUENCE (s1, s2, loc) ->
      SEQUENCE(combine_statement s1, combine_statement s2, loc)
  | IF (exp, s1, s2, loc) ->
      IF(combine_expression exp, combine_substatement s1, combine_substatement s2, loc)
  | WHILE (exp, stat, loc) ->
      WHILE(combine_expression exp, combine_substatement stat, loc)
  | DOWHILE (exp, stat, loc) ->
      DOWHILE(combine_expression exp, combine_substatement stat, loc)
  | FOR (exp1, exp2, exp3, stat, loc) ->
      FOR(combine_expression exp1, combine_expression exp2, combine_expression exp3, combine_substatement stat, loc)
  | BREAK(loc) ->
      BREAK(loc) 
  | CONTINUE (loc)->
      CONTINUE (loc)
  | RETURN (exp, loc) ->
      RETURN (combine_expression exp, loc)
  | SWITCH (exp, stat, loc) ->
      SWITCH(combine_expression exp, combine_substatement stat, loc)
  | CASE (exp, stat, loc) ->
      CASE(combine_expression exp, combine_substatement stat, loc)
  | CASERANGE (expl, exph, stat, loc) ->
      CASERANGE(combine_expression expl, combine_expression exph, 
                combine_substatement stat, loc)
  | DEFAULT (stat, loc) ->
      DEFAULT(combine_substatement stat, loc)
  | LABEL (name, stat, loc) ->
      LABEL(name, combine_substatement stat, loc)
  | GOTO (name, loc) ->
      GOTO(name, loc)
  | ASM (tlist, isvol, outs, ins, clobs, loc) ->
      ASM(tlist, isvol, outs, ins, clobs, loc)   
           
and combine_substatement stat =
  combine_statement stat
  

(*
** Combine and rename declarations
*)
and combine_defs defs global = begin
  (* clear file scope tables *)
  H.clear fDefTable;
  H.clear fMap;
  H.clear fTag; 

  let rec reform_defs = function
      [] -> []
    | def :: rest -> 
      begin
        if global && already_declared def then 
          reform_defs rest (*skip this def *) 
        else
          let combined_def = combine_def def global in
          combined_def :: reform_defs rest
      end
  in 
  reform_defs defs 
end

and tag_defined (s: spec_elem) = 
  match s with
    SpecType t -> begin
      match t with
        Tstruct (id, Some _) ->
          id <> "" &&
          (H.mem structTag id || (H.add structTag id id; false))
      | Tunion (id, Some _) ->
          id <> "" &&
          (H.mem unionTag id || (H.add unionTag id id; false))
      | Tenum (id, Some _) -> 
          id <> "" &&
          (H.mem enumTag id || (H.add enumTag id id; false))
      | _ -> false
    end
  | _ -> false

(* returns true check if def is not declared. Could make another function to 
 * clean this code up. Strip the location from definitions before hashing 
 * them. *)
and already_declared def =
  match def with
    FUNDEF ((s, name), body, loc) ->
      let def' = FUNDEF ((s, name), body, noloc) in
      if isStatic s then
        H.mem fDefTable def' || (H.add fDefTable def' 1; false)
      else
        H.mem gDefTable def' || (H.add gDefTable def' 1; false)

  | DECDEF ((s, names), loc) ->
      let def' = DECDEF ((s, names), noloc) in
      if isStatic s then
          H.mem fDefTable def' || (H.add fDefTable def' 1; false)
      else
        H.mem gDefTable def' || (H.add gDefTable def' 1; false)

  | TYPEDEF ((s, names), loc) ->
      let def' = TYPEDEF ((s, names), noloc) in
      if List.exists tag_defined s then
        true
      else
        H.mem gDefTable def' || (H.add gDefTable def' 1; false)

  | ONLYTYPEDEF (s, loc) ->
      let def' = ONLYTYPEDEF (s, noloc) in
      if List.exists tag_defined s then
        true
      else
        H.mem gDefTable def' || (H.add gDefTable def' 1; false)

  | _ -> false (* ASM and others are always considered to be false *)


(* Find a id name that hasn't been used *)
and find_newId id = findNewName gAlphaTable id

(* declare an id and add that to the mapping table *)
and declare_id (kind: string) (s, (id, dtyp, al)) global 
    : unit =
  let id = if kind = "" then id else kind ^ " " ^ id in
  (*prerr_endline ("declare id: " ^ id);*)
  if global then begin
    if isStatic s then begin
      if not (H.mem fMap id) then (* new declaration *)
        if H.mem gAlphaTable id then begin
            (* needs to be renamed *)
            let newId = find_newId id in 
            H.add gAlphaTable newId id;
            H.add fMap id newId;
        end else begin
          H.add gAlphaTable id id;
           H.add fMap id id
        end
    end else begin
      if not (H.mem gMap id) then (* new declaration *)
        if H.mem gAlphaTable id then begin
          let newId = find_newId id in
          H.add gAlphaTable newId id;
          H.add gMap id newId
        end else begin 
          H.add gAlphaTable id id;
          H.add gMap id id
        end
    end
  end else begin
    if not (H.mem lMap id) && not (H.mem fMap id) && not (H.mem gMap id) then 
      if H.mem gAlphaTable id then
        (* needs to be renamed *)
        H.add lMap id (find_newId id)
      else
        H.add lMap id id
  end

(* declare_ids are used for a name_group. 
   For simplicity, it is calling declare_id *)  
and declare_ids (kind: string) (s, names) global =
  List.iter (fun name -> declare_id kind (s, name) global) names


(* if global, we do a check on duplicate definition.
   Also, declare id (variable, function, and type) *)
and combine_def def global = begin
  
  (* clear function scope mapping table if this is a global def *)
  if global then H.clear lMap; 

  match def with
    FUNDEF ((s, name), body, loc) ->
      (* One MSVC force inline functions to be static. Otherwise the compiler
       * might complain that the function is declared with multiple bodies *)
      let s' =
        if !Cprint.msvcMode && isInline s then
          SpecStorage STATIC :: s else s
      in
      (declare_id "" (s', name) global;
      let n = combine_single_name "" (s', name)  (* force evaluation *)
      in
      FUNDEF(n, List.map combineBlkElem body, loc))

  | DECDEF ((s, names), loc) ->
      List.iter (fun (name, _) -> declare_id "" (s, name) global) names;
      DECDEF((combine_specs s,
             List.map (fun (name, init) -> combine_name "" name,
                                           combine_init_expression init) names), loc)

  | TYPEDEF ((s, names), loc) ->
      (declare_ids "type" (SpecStorage STATIC :: s, names) global;
      TYPEDEF((combine_name_group "type" (s, names)),loc))

  | ONLYTYPEDEF (s, loc) ->
      ONLYTYPEDEF(combine_specs s, loc)

  | GLOBASM (asm,loc) ->
      GLOBASM (asm, loc)

  | PRAGMA (a,loc) ->
      PRAGMA (a, loc)    
end

  
(* look up id from Mapping tables *)
and lookup_id (kind: string) id = begin
  let id' = if kind = "" then id else kind ^ " " ^ id in
  let newid' = 
    try
      H.find lMap id'
    with Not_found ->
      try
        H.find fMap id'
      with Not_found ->
        try
          H.find gMap id'
        with Not_found ->
          (*prerr_endline ("Undeclared id: " ^ id);*)
          id'
  in
  if kind = "" then 
    newid'
  else
    let lk = String.length kind in
    let lnew = String.length newid' in
    if lnew <= lk + 1 then 
      E.s (E.bug "lookup_id: %s" id');
    String.sub newid' (lk + 1) (lnew - lk - 1)
end
        
let combine (files : Cabs.file list) : Cabs.file =
begin
   List.flatten (List.map 
                  (fun defs' -> combine_defs defs' true) 
                  files)
end

























