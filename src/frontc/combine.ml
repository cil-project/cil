(* Things may need to be changed
   ATTRIBUTE --- For now, Raymond is ignoring them, and actually,
                 attributes are not printed at all.
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

(* Hash tables for combiner *)

(* Tables that used to keep track of re-definitions 
 - store definitions in tables *)
let gDefTable = Hashtbl.create 307 (* global definition *)
let fDefTable = Hashtbl.create 107 (* static global definition (file scope) *)

let gAlphaTable = Hashtbl.create 307 (* id names that have been used *)

(* Table that maps id to renamed id *)
let gMap = Hashtbl.create 107 (* global id *)
let fMap = Hashtbl.create 107 (* file scope id *)
let lMap = Hashtbl.create 107 (* function scope id *)

let gTagUsedTable = Hashtbl.create 107 (* keep track tag names that have been used *)
let fTag = Hashtbl.create 107 (* tags that have appeared in file *)
let gTag = Hashtbl.create 107 (* tags that have appeared in ALL files *)

let structTag = Hashtbl.create 107
let unionTag = Hashtbl.create 107


let rec combine_type typ =
  match typ with
    NO_TYPE -> NO_TYPE
  | VOID -> VOID
  | INT (size, sign) -> INT(size, sign)
  | BITFIELD (bt, exp) -> BITFIELD(combine_type bt, combine_expression exp)
  | FLOAT size -> FLOAT size
  | DOUBLE size -> DOUBLE size
  | NAMED_TYPE id -> NAMED_TYPE (lookup_id id)
  | ENUM id -> ENUM (lookup_tag id)
  | ENUMDEF (id, items) -> ENUMDEF (lookup_tag id, items)
  | STRUCT id -> STRUCT (lookup_tag id)
  | STRUCTDEF (id, flds) -> STRUCTDEF (lookup_tag id, combine_fields flds)
  | UNION id -> UNION(lookup_tag id)
  | UNIONDEF (id, flds) -> UNIONDEF(lookup_tag id, combine_fields flds) 
  | PROTO (typ, pars, ell, x) -> PROTO(combine_type typ, combine_params pars, ell, x)
  | OLD_PROTO (typ, pars, ell, x) -> OLD_PROTO(combine_type typ, combine_old_params pars, ell, x)
  | PTR typ -> PTR(combine_type typ)
  | ARRAY (typ, dim) -> ARRAY(combine_type typ, combine_expression dim)
(*
  | CONST typ -> CONST(combine_type typ)
  | VOLATILE typ -> VOLATILE(combine_type typ)
*)
  | ATTRTYPE (typ, a) -> 
      let combineOne (s, el) =
        (s, List.map combine_expression el)
      in 
      let rec doconstvol = function
          [] -> []
        | ("const", []) :: rest -> ("const", []) :: doconstvol rest
        | ("volatile", []) :: rest -> ("volatile", []) :: doconstvol rest
        | a :: rest -> combineOne a :: doconstvol rest
      in
      ATTRTYPE(combine_type typ, doconstvol a)

  | TYPEOF e -> TYPEOF(combine_expression e)

and combine_fields flds = begin
  let ids = List.flatten (List.map (fun name_group -> 
                match name_group with (typ, sto, names) -> 
                (List.map (fun (id, typ, attr, exp) -> id) names)) flds) in
  begin
    List.iter (fun id -> Hashtbl.add lMap id id) ids;
    let flds' = List.map (fun fld -> combine_name_group fld) flds in 
    List.iter (fun id -> Hashtbl.remove lMap id) ids;
    flds'
  end
end

(* keep adding "_" until we find one that has not been used *)
and find_newTag tag = begin
  let tmp_tag = ref "" and flag = ref true in
  begin
    tmp_tag := tag;
    while !flag do
      (try 
         Hashtbl.find gTagUsedTable !tmp_tag;
         tmp_tag := (!tmp_tag ^ "_");
       with Not_found -> flag := false;)
    done;
    Hashtbl.add gTagUsedTable !tmp_tag tag;
    !tmp_tag
  end
end

(* return empty string if tag begins with __anon
   lookup from hashtable otherwise  *)
and lookup_tag tag = begin
  if ((String.length tag) >= 6 && (String.sub tag 0 6) = "__anon") then 
    ((*prerr_endline "Found __anon tag";*) "")
  else
    try
      Hashtbl.find fTag tag
    with Not_found ->
      (try
         Hashtbl.find gTag tag
       with Not_found -> (* tag has not been used at all *)
         (Hashtbl.add gTag tag tag;
          Hashtbl.add fTag tag tag;
          tag))
  
end

(* declare a tag. NOTE: this is not being used at all right now *)
and declare_tag tag = begin
   prerr_endline ("declare tag: " ^ tag);
   try
     ignore(Hashtbl.find fTag tag)
   with Not_found ->
     let newTag = find_newTag tag in 
     begin
       Hashtbl.add fTag tag newTag;
       Hashtbl.add gTag tag newTag;
     end     
end

        
and combine_onlytype typ =
  combine_type typ

(* NOTE: REMOVE ATTRIBUTE *)    
and combine_name ((id, typ, attr, exp) : name) = begin
 if id = "___missing_field_name"
  then
    (id, combine_type typ, [], combine_expression exp)
  else
    (lookup_id id, combine_type typ, [], combine_expression exp)
end
        
and combine_name_group (typ, sto, names) =
  (combine_type typ, sto, List.map combine_name names)
    
and combine_single_name (typ, sto, name) =
  (combine_type typ, sto, combine_name name) 

(* Raymond added declare_id lookup_id *)
and combine_params (pars : single_name list) = begin
  List.iter (fun name -> declare_id name false) pars;
  List.map (fun single_name -> combine_single_name single_name) pars
end

(* Raymond added declare_id and lookup_id here *)    
and combine_old_params pars = begin
  List.iter (fun id -> declare_id (NO_TYPE, NO_STORAGE, (id, NO_TYPE, [], NOTHING)) false) pars;
  List.map lookup_id pars
end
        
and combine_exps exps =
  List.map combine_expression exps

(* No need to rename fields *)    
and combine_expression (exp : expression) =
  match exp with
    NOTHING ->
      NOTHING
  | UNARY (op, exp') ->
      UNARY(op, combine_expression exp')  
  | BINARY (op, exp1, exp2) ->
      BINARY(op, combine_expression exp1, combine_expression exp2)      
  | QUESTION (exp1, exp2, exp3) ->
      QUESTION(combine_expression exp1, combine_expression exp2, combine_expression exp3)    
  | CAST (typ, exp) ->
      CAST(combine_type typ, combine_expression exp)     
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
      | CONST_STRING s -> CONST_STRING s
      | CONST_COMPOUND initexps ->
          let doinitexp = function
              NO_INIT, e -> (NO_INIT, combine_expression e)
            | i, e -> 
                let rec doinit = function
                    NO_INIT -> NO_INIT
                  | FIELD_INIT (fn, i) -> FIELD_INIT(fn, doinit i)
                  | INDEX_INIT (e, i) -> 
                      INDEX_INIT(combine_expression e, doinit i)                              in
                (doinit i, combine_expression e)
          in
          CONST_COMPOUND(List.map doinitexp initexps)
        ))

  | VARIABLE name -> 
      VARIABLE(lookup_id name)
  | EXPR_SIZEOF exp ->
      EXPR_SIZEOF (combine_expression exp)
  | TYPE_SIZEOF typ ->
      TYPE_SIZEOF(combine_type typ)
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
    NOP ->
      NOP
  | COMPUTATION exp ->
      COMPUTATION(combine_expression exp)  
  | BLOCK blk ->
      BLOCK(List.map combineBlkElem blk)
  | SEQUENCE (s1, s2) ->
      SEQUENCE(combine_statement s1, combine_statement s2)
  | IF (exp, s1, s2) ->
      IF(combine_expression exp, combine_substatement s1, combine_substatement s2)
  | WHILE (exp, stat) ->
      WHILE(combine_expression exp, combine_substatement stat)
  | DOWHILE (exp, stat) ->
      DOWHILE(combine_expression exp, combine_substatement stat)
  | FOR (exp1, exp2, exp3, stat) ->
      FOR(combine_expression exp1, combine_expression exp2, combine_expression exp3, combine_substatement stat)
  | BREAK ->
      BREAK
  | CONTINUE ->
      CONTINUE
  | RETURN exp ->
      RETURN (combine_expression exp) 
  | SWITCH (exp, stat) ->
      SWITCH(combine_expression exp, combine_substatement stat)
  | CASE (exp, stat) ->
      CASE(combine_expression exp, combine_substatement stat)
  | DEFAULT stat ->
      DEFAULT(combine_substatement stat)
  | LABEL (name, stat) ->
      LABEL(name, combine_substatement stat)
  | GOTO name ->
      GOTO(name)    
  | ASM (tlist, isvol, outs, ins, clobs) ->
      ASM(tlist, isvol, outs, ins, clobs)   
           
and combine_substatement stat =
  combine_statement stat
  

(*
** Combine and rename declarations
*)
and combine_defs defs global = begin
  (* clear file scope tables *)
  Hashtbl.clear fDefTable;
  Hashtbl.clear fMap;
  Hashtbl.clear fTag; 

  let rec reform_defs = function
      [] -> []
    | def :: rest -> 
      begin
        if global && (already_declared (remove_anon def)) then 
          (reform_defs rest) (*skip this def *) 
        else
          let combined_def = combine_def def global in
            combined_def :: (reform_defs rest)
      end
  in 
  reform_defs defs 
end

(* A set of remove_anon functions to remove _anon from 
   TYPEDEF and ONLYTYPEDEF for the purposes of checking
   duplicate definition.
   If adding tag _anon is removed from Cparser, then we don't
   need these functions *)

and remove_anon_name_group (typ, sto, names) = begin
  (remove_anon_type typ, sto, List.map remove_anon_name names)
end

and remove_anon_id id = begin
  
  if (((String.length id) >= 6) && ((String.sub id 0 6) = "__anon")) then
    ""
  else
    id
end

and remove_anon_type typ = begin
  match typ with
    STRUCTDEF (id, flds) ->
      STRUCTDEF (remove_anon_id id,
                 List.map remove_anon_name_group flds)
  | UNIONDEF (id, flds) ->
      UNIONDEF (remove_anon_id id,
              List.map remove_anon_name_group flds)
  | ENUMDEF (id, items) -> (* Occur in onlytypedef, not typedef *)
      ENUMDEF (remove_anon_id id, items)
  | _ -> typ
end    

and remove_anon_name (id, typ, attr, exp) = begin
  (id, remove_anon_type typ, attr, exp)
end  
      
and remove_anon def = begin
  match def with
    TYPEDEF (typ, sto, names) ->
      TYPEDEF (remove_anon_type typ, sto, List.map remove_anon_name names)
  | ONLYTYPEDEF (typ, sto, names) ->
      ONLYTYPEDEF (remove_anon_type typ, sto, List.map remove_anon_name names)
  | _ -> def
 
end        

and tag_defined (typ, sto, names) = begin
  match typ with
    STRUCTDEF (id, _) ->
      begin
      if id = "" then false
      else
      (try
         ignore(Hashtbl.find structTag id);
         true
       with Not_found ->
         (Hashtbl.add structTag id id;
          false))
      end
  | UNIONDEF (id, _) ->
      if id = "" then false
      else 
      (try
         ignore(Hashtbl.find unionTag id);
         true
       with Not_found ->
         (Hashtbl.add unionTag id id;
          false))
  | _ -> false
end

(* returns true check if def is not declared
   Could make another function to clean this code up *)
and already_declared def = begin
  match def with
    FUNDEF ((typ, sto, name), body) ->
      (match sto with
        STATIC i ->
          (try 
            Hashtbl.find fDefTable def;
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))            
      | _ -> 
          (try
            Hashtbl.find gDefTable def;
            true
          with Not_found -> 
              (Hashtbl.add gDefTable def 1;
              false)))
              
  | OLDFUNDEF ((typ, sto, name), decs, body) ->
      (match sto with
        STATIC i ->
          (try 
            ignore(Hashtbl.find fDefTable def);
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))
      | _ -> 
          (try
            ignore(Hashtbl.find gDefTable def);
            true
          with Not_found ->
              (Hashtbl.add gDefTable def 1;
              false)))
  
  | DECDEF (typ, sto, names) ->
      (match sto with
        STATIC i ->
          (try 
            ignore(Hashtbl.find fDefTable def);
            true
          with Not_found ->
              (Hashtbl.add fDefTable def 1;
              false))
      | _ -> 
          (try
            ignore(Hashtbl.find gDefTable def);
            true
          with Not_found -> 
              (Hashtbl.add gDefTable def 1;
              false)))
              
  | TYPEDEF names ->
      if tag_defined names then
        true
      else
        (try
          ignore(Hashtbl.find gDefTable def);
          true
        with Not_found ->
            (Hashtbl.add gDefTable def 1;
            false))
          
  | ONLYTYPEDEF names ->
      if tag_defined names then
        true
      else 
        (try
          ignore(Hashtbl.find gDefTable def);
          true
        with Not_found ->
            (Hashtbl.add gDefTable def 1;
            false))
      
  | _ -> false (* ASM and others are always considered to be false *)
end

(* Find a id name that hasn't been used *)
and find_newId id = begin
  let tmp_id = ref "" and flag = ref true in 
  begin
    tmp_id := id;
    while !flag do
      try
        Hashtbl.find gAlphaTable !tmp_id;
        tmp_id := (!tmp_id ^ "_");
      with Not_found -> flag := false;
    done;
    !tmp_id
  end
end

(* declare an id and add that to the mapping table *)
and declare_id (typ, sto, (id, typ', attr, exp)) global = begin
  (*prerr_endline ("declare id: " ^ id);*)
  if global then 
    begin
      match sto with
        STATIC i ->
          (try
             Hashtbl.find fMap id; ();
           with Not_found -> (* new declaration *)
             (try
                begin
                  Hashtbl.find gAlphaTable id;
                  (* needs to be renamed *)
                  let newId = find_newId id in 
                  begin
                    Hashtbl.add gAlphaTable newId id;
                    Hashtbl.add fMap id newId;
                  end
                end
                with Not_found ->
                  (Hashtbl.add gAlphaTable id id;
                   Hashtbl.add fMap id id;)))
      | _ -> 
          (try 
             Hashtbl.find gMap id; ();
           with Not_found -> (* new declaration *)
             (try
                ignore(Hashtbl.find gAlphaTable id);
                let newId = find_newId id in
                begin
                  Hashtbl.add gAlphaTable newId id;
                  Hashtbl.add gMap id newId;
                end
                with Not_found ->
                  (Hashtbl.add gAlphaTable id id;
                   Hashtbl.add gMap id id;)))
    end
  else
    begin (* try to reuse the names as many times as possible *)
      try 
        ignore(Hashtbl.find lMap id);
      with Not_found -> 
        (try
           ignore(Hashtbl.find fMap id);
         with Not_found ->
           (try
              ignore(Hashtbl.find gMap id);
            with Not_found ->
              (try
                 ignore(Hashtbl.find gAlphaTable id);
                 (* needs to be renamed *)
                 Hashtbl.add lMap id (find_newId id);
               with Not_found ->
                 Hashtbl.add lMap id id;)))
    end
end

(* declare_ids are used for a name_group. 
   For simplicity, it is calling declare_id *)  
and declare_ids (typ, sto, names) global = begin      
  List.iter (fun name -> declare_id (typ, sto, name) global) names
end

(* if global, we do a check on duplicate definition.
   Also, declare id (variable, function, and type) *)
and combine_def def global = begin
  
  (* clear function scope mapping table if this is a global def *)
  if global then 
    begin
      Hashtbl.clear lMap; 
    end;

  match def with
    FUNDEF (name, body) ->
      (declare_id name global;
      FUNDEF(combine_single_name name, List.map combineBlkElem body))
               
  | OLDFUNDEF (name, decs, body) ->
      (declare_id name global;
      OLDFUNDEF(combine_single_name name, List.map (fun dec -> combine_name_group dec) decs, List.map combineBlkElem body))
       
  | DECDEF names ->
      (declare_ids names global;
      DECDEF(combine_name_group names))
       
  | TYPEDEF (typ, sto, names) ->
      (declare_ids (typ, STATIC true, names) global;
      TYPEDEF(combine_name_group (typ, sto, names)))
      
  | ONLYTYPEDEF (typ, sto, names) ->
      (declare_ids (typ, STATIC true, names) global;
      ONLYTYPEDEF(combine_name_group (typ, sto, names)))
        
  | GLOBASM asm -> 
      GLOBASM asm 

  | PRAGMA a -> 
      PRAGMA a
end
  
(* look up id from Mapping tables *)
and lookup_id id = begin
  try
    Hashtbl.find lMap id
  with Not_found ->
    try
      Hashtbl.find fMap id
    with Not_found ->
      try
        Hashtbl.find gMap id
      with Not_found ->
        (*prerr_endline ("Undeclared id: " ^ id);*)
        id
end
        
let combine (files : Cabs.file list) : Cabs.file =
  List.flatten (List.map 
                  (fun defs' -> combine_defs defs' true) 
                  files)

