(* A module that scans a CIL file and tags all TPtr with a unique attribute 
 * id. It also constructs a mapping from attributes id to places where they 
 * were introduced  *)
open Cil
open Pretty

module H = Hashtbl
module E = Errormsg

module N = Ptrnode

let lu = locUnknown

let currentFileName = ref ""
let currentFunctionName = ref ""
let currentResultType = ref voidType

let callId = ref (-1)  (* Each call site gets a new ID *)

(* Grab the node from the attributs of a type. Returns dummyNode if no such 
 * node *)
let nodeOfType t = 
  match unrollType t with 
    TPtr(_, a) -> begin
      match N.nodeOfAttrlist a with
        Some n -> n
      | None -> N.dummyNode
    end
  | _ -> N.dummyNode


(* Rewrite types so that pointer types get a new node and a new attribute. 
 * The attribute is added by the N.newNode.  *)

(* Keep track of composite types that we have done already, to avoid looping *)
let doneComposites : (int, bool) H.t = H.create 111 

(* Pass also the place and the next index within the place. Returns the 
 * modified type and the next ununsed index *)
let rec doType (t: typ) (p: N.place) 
               (nextidx: int) : typ * int = 
  match t with 
    (TVoid _ | TInt _ | TFloat _ | TBitfield _ | TEnum _ ) -> t, nextidx
  | TPtr (bt, a) -> begin
      match N.nodeOfAttrlist a with
        Some n -> TPtr (bt, a), nextidx (* Already done *)
      | None -> 
          let bt', i' = doType bt p (nextidx + 1) in
          let n = N.newNode p nextidx bt' a in
          TPtr (bt', n.N.attr), i'
  end
  | TArray(bt, len, a) ->
      let bt', i' = doType bt p nextidx in
      if bt == bt' then t, i' 
      else TArray(bt', len, a), i'
          
  | TComp comp -> 
      if H.mem doneComposites comp.ckey then
        t, nextidx
      else begin
        H.add doneComposites comp.ckey true; (* before we do the fields *)
        List.iter 
          (fun f -> 
            let t', i' = doType f.ftype (N.PField f) 0 in
            f.ftype <- t') comp.cfields;
        t, nextidx
      end
        
  | TNamed (n, bt, a) -> 
      let t', _ = doType bt (N.PType n) 0 in
      t', nextidx
        
  | TForward (comp, a) -> 
      if H.mem doneComposites comp.ckey then
        t, nextidx
      else begin
        H.add doneComposites comp.ckey true; (* before we do the fields *)
        List.iter 
          (fun f -> 
            let t', i' = doType f.ftype (N.PField f) 0 in
            f.ftype <- t') comp.cfields;
        t, nextidx
      end
        
  | TFun (restyp, args, isva, a) -> 
      let restyp', i0 = doType restyp p nextidx in
          (* Rewrite the argument types in place *)
      let i' = 
        List.fold_left 
          (fun nidx arg -> 
            let t', i' = doType arg.vtype p nidx in
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
  N.addEdge n next (-1);
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


(* Compute the sign of an expression. Extend this to a real constant folding 
 * + the sign rule  *)
type sign = SPos | SNeg | SAny | SLiteral of int

let rec signOf = function
    Const(CInt(n, _, _), _) -> SLiteral n
  | Const(CChr c, _) -> SLiteral (Char.code c)
  | SizeOf _ -> SPos (* We do not compute it now *)
  | UnOp (Neg, e, _, _) -> begin
      match signOf e with
        SPos -> SNeg
      | SLiteral n -> SLiteral (- n)
      | SNeg -> SNeg
      | _ -> SAny
  end
  | UnOp (LNot, e, _, _) -> SPos
  | BinOp (PlusA, e1, e2, _, _) -> begin
      match signOf e1, signOf e2 with
        SPos, SPos -> SPos
      | SLiteral n, SPos when n >= 0 -> SPos
      | SPos, SLiteral n when n >= 0 -> SPos
      | SLiteral n1, SLiteral n2 -> SLiteral (n1 + n2)
      | SNeg, SNeg -> SNeg
      | SLiteral n, SNeg when n <= 0 -> SNeg
      | SNeg, SLiteral n when n <= 0 -> SNeg
      | _ -> SAny
  end
  | BinOp (MinusA, e1, e2, _, _) -> begin
      match signOf e1, signOf e2 with
        SPos, SNeg -> SPos
      | SLiteral n, SNeg when n >= 0 -> SPos
      | SPos, SLiteral n when n <= 0 -> SPos
      | SLiteral n1, SLiteral n2 -> SLiteral (n1 - n2)
      | SNeg, SPos -> SNeg
      | SLiteral n, SPos when n <= 0 -> SNeg
      | SNeg, SLiteral n when n >= 0 -> SNeg
      | _ -> SAny
  end
  | _ -> SAny

(* Do varinfo. We do the type and for all variables we also generate a node 
 * that will be used when we take the address of the variable (or if the 
 * variable contains an array) *)
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
    
(* Do an expression. Return an expression, a type and a node. The node is 
 * only meaningful if the type is a TPtr _. In that case the node is also 
 * refered to from the attributes of TPtr  *)
let rec doExp (e: exp) = 
  match e with 
    Lval lv -> 
      let lv', lvn = doLvalue lv false in
      Lval lv', lvn.N.btype, nodeOfType lvn.N.btype

  | AddrOf (lv, l) -> 
      let lv', lvn = doLvalue lv false in
      AddrOf (lv', l), TPtr(lvn.N.btype, lvn.N.attr), lvn

  | StartOf lv -> 
      let lv', lvn = doLvalue lv false in (* Not quite right !!! *)
      StartOf lv', TPtr(lvn.N.btype, lvn.N.attr), lvn

  | UnOp (uo, e, tres, l) -> (* tres is an arithmetic type *)
      UnOp(uo, doExpAndCast e tres, tres, l), tres, N.dummyNode

  | SizeOf (t, l) ->
      let t', _ = doType t (N.anonPlace()) 0 in
      SizeOf (t', l), uintType, N.dummyNode

        (* arithemtic binop *)
  | BinOp (((PlusA|MinusA|Mult|Div|Mod|Shiftlt|Shiftrt|Lt|Gt|Le|Ge|Eq|Ne|BAnd|BXor|BOr|LtP|GtP|LeP|GeP|EqP|NeP|MinusPP) as bop), 
           e1, e2, tres, l) -> 
             BinOp(bop, doExpAndCast e1 tres,
                   doExpAndCast e2 tres, tres, l), tres, N.dummyNode
       (* pointer arithmetic *)
  | BinOp (((PlusPI|MinusPI) as bop), e1, e2, tres, l) -> 
      let e1', e1t, e1n = doExp e1 in
      (match signOf 
          (match bop with PlusPI -> e2 | _ -> UnOp(Neg, e2, intType, lu)) with
        SLiteral 0 -> ()
      | SPos -> e1n.N.posarith <- true
      | SLiteral n when n > 0 -> e1n.N.posarith <- true
      | _ -> e1n.N.arith <- true);
      BinOp (bop, e1', doExpAndCast e2 intType, e1t, l), e1t, e1n
      
      
  | CastE (newt, e, l) -> 
      let newt', _ = doType newt (N.anonPlace ()) 0 in
      CastE (newt', doExpAndCast e newt', l), newt', nodeOfType newt'

  | _ -> (e, typeOf e, N.dummyNode)


(* Do an lvalue. We assume conservatively that this is for the purpose of 
 * taking its address. Return a modifed lvalue and a node that stands for & 
 * lval. Just ignore the node and get its base type if you do not want to 
 * take the address of. *)
and doLvalue ((base, off) : lval) (iswrite: bool) : lval * N.node = 
  let base', startNode = 
    match base with 
      Var vi -> begin 
        doVarinfo vi;
        (* Now grab the node for it *)
        base, 
        (match N.nodeOfAttrlist vi.vattr with Some n -> n | _ -> N.dummyNode)
      end
    | Mem e -> 
        let e', et, ne = doExp e in
        if iswrite then
          ne.N.updated <- true;
        Mem e', ne
  in
  let newoff, newn = doOffset off startNode in
  (base', newoff), newn
        
(* Now do the offset. Base types are included in nodes. *)
and doOffset (off: offset) (n: N.node) : offset * N.node = 
  match off with 
    NoOffset -> off, n
  | Field(fi, resto) -> 
      let nextn = fieldOfNode n fi in
      let newo, newn = doOffset resto nextn in
      Field(fi, newo), newn
  | First resto -> 
      let nextn = firstOfNode n in
      let newo, newn = doOffset resto nextn in
      First newo, newn
  | Index(e, resto) -> begin
      n.N.posarith <- true;
      let newo, newn = doOffset resto n in
      let e', et, _ = doExp e in
      Index(e', newo), newn
  end


  
(* Now model an assignment of a processed expression into a type *)
and expToType (e,et,en) t (callid: int) = 
  let rec isZero = function
      Const(CInt(0, _, _), _) -> true
    | CastE(_, e, _) -> isZero e
    | _ -> false
  in
  let etn = nodeOfType et in
  let tn  = nodeOfType t in
  match etn == N.dummyNode, tn == N.dummyNode with
    true, true -> e
  | false, true -> e (* Ignore casts of pointer to non-pointer *)
  | false, false -> 
      if isZero e then 
        tn.N.null <- true (* Do not add an edge *)
      else
        N.addEdge etn tn callid; e
  | true, false -> 
      (* Cast of non-pointer to a pointer. Check for zero *)
      (if isZero e then
        tn.N.null <- true
      else
        tn.N.intcast <- true);
      e
    
and doExpAndCast e t = 
  expToType (doExp e) t (-1)

and doExpAndCastCall e t callid = 
  expToType (doExp e) t callid

(* Do a statement *)
let rec doStmt (s: stmt) = 
  match s with 
    (Skip | Label _ | Case _ | Default | Break | Continue | Goto _) -> s
  | Sequence sl -> Sequence (List.map doStmt sl)
  | Loop s -> Loop (doStmt s)
  | IfThenElse (e, s1, s2) -> 
      IfThenElse (doExpAndCast e intType, doStmt s1, doStmt s2)
  | Switch (e, s) -> Switch (doExpAndCast e intType, doStmt s)
  | Return None -> s
  | Return (Some e) -> 
      Return (Some (doExpAndCast e !currentResultType))
  | Instr (Asm _) -> s
  | Instr (Set (lv, e, l)) -> 
      let lv', lvn = doLvalue lv true in
      let eres = doExp e in
      (* Now process the copy *)
      let e' = expToType eres lvn.N.btype (-1) in
      Instr (Set (lv', e', l))

  | Instr (Call (reso, func, args, l)) -> 
      let func', funct, funcn = doExp func in
      let (rt, formals, isva) = 
        match unrollType funct with
          TFun(rt, formals, isva, _) -> rt, formals, isva
        | _ -> E.s (E.bug "Call to a non-function")
      in
      incr callId; (* A new call id *)
          (* Now check the return value*)
      (match reso, unrollType rt with
        None, TVoid _ -> ()
      | Some _, TVoid _ -> ignore (E.warn "Call of subroutine is assigned")
      | None, _ -> () (* "Call of function is not assigned" *)
      | Some destvi, _ -> 
          N.addEdge (nodeOfType rt) (nodeOfType destvi.vtype) !callId);
          (* Now check the arguments *)
      let rec loopArgs formals args = 
        match formals, args with
          [], _ when (isva || args = []) -> args
        | fo :: formals, a :: args -> 
            let a' = doExpAndCastCall a fo.vtype !callId in
            a' :: loopArgs formals args
        | _, _ -> E.s (E.bug "Not enough arguments")
      in
      Instr (Call(reso, func', loopArgs formals args, l))


  
     
  
      
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
        | Some i -> Some (doExpAndCast i vi.vtype)
      in
      GVar (vi, init')
  | GFun fdec -> 
      doVarinfo fdec.svar;
      currentFunctionName := fdec.svar.vname;
      (match fdec.svar.vtype with
        TFun(rt, _, _, _) -> currentResultType := rt
      | _ -> E.s (E.bug "Not a function"));
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
  output_string c "// Now the graph\n";
  N.gc (); 
  N.printGraph c;
  d_attrcustom := ocustom
    
