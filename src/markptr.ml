(* A module that scans a CIL file and tags all TPtr with a unique attribute 
 * id. It also constructs a mapping from attributes id to places where they 
 * were introduced  *)
open Cil
open Pretty

module H = Hashtbl
module E = Errormsg

module N = Ptrnode


let currentFileName = ref ""
let currentFunctionName = ref ""
let currentResultType = ref voidType

let nodeOfAttrlist al = 
  match filterAttributes "_ptrnode" al with
    [] -> None
  | [ACons(_, [AInt n])] -> begin
      try Some (H.find N.idNode n)
      with Not_found -> E.s (E.bug "Cannot find node with id = %d\n" n)
  end
  | _ -> E.s (E.bug "nodeOfAttrlist")


(* Rewrite types so that pointer types get a new attribute and a new node *)

(* Keep track of types that we have done already *)
let rec doType (t: typ) (p: N.place) (nextidx: int) : typ * int = 
  match t with 
    (TVoid _ | TInt _ | TFloat _ | TBitfield _ | TEnum _ ) -> t, nextidx
  | TPtr (bt, a) -> begin
      let bt', i' = doType bt p nextidx in
      match nodeOfAttrlist a with
        Some n -> TPtr (bt', a), i' (* Already done *)
      | None -> 
          let n = N.newNode p i' bt' a in
          TPtr (bt', n.N.attr), 1 + i'
  end
  | TArray(bt, len, a) ->
      let bt', i' = doType bt p nextidx in
      if bt == bt' then t, i' 
      else TArray(bt', len, a), i'
          
  | TComp comp -> 
      List.iter 
        (fun f -> 
          let t', i' = doType f.ftype (N.PField f) 0 in
          f.ftype <- t') comp.cfields;
      t, nextidx
        
  | TNamed (n, bt, a) -> doType bt (N.PType n) 0
        
  | TForward (comp, a) -> 
      List.iter 
        (fun f -> 
          let t', i' = doType f.ftype (N.PField f) 0 in
          f.ftype <- t') comp.cfields;
      t, nextidx
        
  | TFun (restyp, args, isva, a) -> 
      let restyp', i0 = doType restyp p nextidx in
          (* Rewrite the argument types in place *)
      let i' = 
        List.fold_left 
          (fun nidx arg -> 
            let t', i' = doType restyp p nidx in
            arg.vtype <- t';
            i') i0 args in
      TFun(restyp', args, isva, a), i'

          

(* For each node corresponding to a struct or union or array type we will 
 * create successor node corresponding to various offsets. We cache these 
 * nodes indexed by the start node id and the name of the field. In the 
 * particular case of an array, we use the field name "@field" to refer to 
 * the first element *)
let offsetNodes : (int * string, N.node) H.t = H.create 111

(* Create a new offset node *)
let newOffsetNode (n: N.node) (fname: string) 
                  (btype: typ) (battr: attribute list) = 
  let next = N.newNode (N.POffset(n.N.id, fname)) 0 btype battr in
  (* Add an edge *)
  N.addEdge n next;
  next

(* Create a field successor *)
let fieldOfNode (n: N.node) (fi: fieldinfo) = 
  try
    H.find offsetNodes (n.N.id, fi.fname)
  with Not_found -> 
    newOffsetNode n fi.fname fi.ftype []

(* Create a first successor (for arrays) *)
let firstOfNode (n: N.node) = 
  try
    H.find offsetNodes (n.N.id, "@first")
  with Not_found -> begin
    let bt = 
      match unrollType n.N.btype with
        TArray(bt, _, _) -> bt
      | _ -> E.s (E.bug "firstOfNode on a non-array")
    in
    newOffsetNode n "@first" bt []
  end
    
(* Now do the offset. Base types are included in nodes. *)
let rec doOffset (off: offset) (n: N.node) : N.node = 
  match off with 
    NoOffset -> n
  | Field(fi, resto) -> 
      let nextn = fieldOfNode n fi in
      doOffset resto nextn
  | First resto -> 
      let nextn = firstOfNode n in
      doOffset resto nextn
  | Index(e, resto) -> begin
      n.N.index <- true;
      doOffset resto n
  end

(* Do an expression. Return an expression, a type and a node. The node is 
 * only meaningful if the type is a TPtr _  *)
let rec doExp (e: exp) = e


(* Now model an assignment of a processed expression into a type *)
let expToType (e,et,en) t = e
    

(* Do a statement *)
let rec doStmt (s: stmt) = 
  match s with 
    (Skip | Label _ | Case _ | Default | Break | Continue | Goto _) -> s
  | Sequence sl -> Sequence (List.map doStmt sl)
  | Loop s -> Loop (doStmt s)
  | IfThenElse (e, s1, s2) -> 
      IfThenElse (expToType (doExp e) intType, doStmt s1, doStmt s2)
  | Switch (e, s) -> Switch (expToType (doExp e) intType, doStmt s)
  | Return None -> s
  | Return (Some e) -> 
      Return (Some (expToType (doExp e) !currentResultType))
  | Instr (Asm _) -> s
  | Instr (Set (lv, e, _)) -> s
  | Instr (Call (reso, func, args, _)) -> s
  
     
(* Do varinfo *)
let doVarinfo vi = 
  (* Compute a place for it *)
  let place = 
    if vi.vglob then
      if vi.vstorage = Static then 
        N.PStatic (!currentFileName, vi.vname)
      else
        N.PGlob vi.vname
    else
      N.PLocal (!currentFileName, !currentFunctionName, vi.vname)
  in
  (* Do the type of the variable. Start the index at 1 *)
  let t', _ = doType vi.vtype place 1 in
  vi.vtype <- t';
  (* Associate a node with the variable itself. Use index = 0 *)
  let n = N.getNode place 0 vi.vtype [] in
  (* Add this to the variable attributes *)
  vi.vattr <- addAttribute (ACons("_ptrnode", [AInt n.N.id])) vi.vattr
  
      
(* Now do the globals *)
let doGlobal (g: global) : global = 
  match g with
    (GText _ | GPragma _ | GAsm _) -> g
  | GType (n, t) -> 
      let t', _ = doType t (N.PType n) 0 in
      GType (n, t')
  | GDecl vi -> doVarinfo vi; g
  | GVar (vi, init) -> 
      doVarinfo vi;
      let init' = 
        match init with
          None -> None
        | Some i -> Some (doExp i)
      in
      GVar (vi, init')
  | GFun fdec -> 
      doVarinfo fdec.svar;
      currentFunctionName := fdec.svar.vname;
      (match fdec.svar.vtype with
        TFun(rt, _, _, _) -> currentResultType := rt
      | _ -> E.s (E.bug "No a function"));
      (* Do the formals (the local version). Reuse the types from the 
       * function type *)
      List.iter doVarinfo fdec.sformals;
      (* Do the other locals *)
      List.iter doVarinfo fdec.slocals;
      (* Do the body *)
      fdec.sbody <- doStmt fdec.sbody;
      g
      
      
(* Now do the file *)      
let markFile fl = 
  currentFileName := fl.fileName;
  {fl with globals = List.map doGlobal fl.globals}

        


(* A special file printer *)
let printFile (c: out_channel) fl = 
  let ocustom = !d_attrcustom in
  let myAttrCustom = function
      ACons("_ptrnode", [AInt n]) -> Some (dprintf "NODE(%d)" n)
    | AId("_ronly") -> Some (text "RONLY")
    | AId("_safe") -> Some (text "SAFE")
    | AId("_seq") -> Some (text "SEQ")
    | AId("_index") -> Some (text "INDEX")
    | AId("_stack") -> Some (text "STACK")
    | AId("_opt") -> Some (text "OPT")
    | AId("_wild") -> Some (text "WILD")
    | a -> ocustom a
  in
  d_attrcustom := myAttrCustom;
  Cil.printFile c fl;
  d_attrcustom := ocustom;
  output_string c "// Now the graph\n";
  N.printGraph c
    
