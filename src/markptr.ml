open Cil
open Pretty

module H = Hashtbl
module E = Errormsg

module N = Ptrnode

let lu = locUnknown

let currentFileName = ref ""
let currentFunctionName = ref ""
let currentResultType = ref voidType

let boxing = ref true

let callId = ref (-1)  (* Each call site gets a new ID *)

let polyId = ref (-1) 

(* A number of functions will be treated polymorphically. In that case store 
 * their original type. When we process the type of a polymorphic function we 
 * first store here the original type.  *)
let polyFunc : (string, typ option ref) H.t = H.create 7

(* all of the printf-like format string functions. The integer is the
 * argument number of the format string. *)
let printfFunc : (string, int ) H.t = H.create 15


(* if some enclosing context [like the attributes for a field] says that
 * this array should be sized ... we do not want to forget it! *)
let addArraySizedAttribute arrayType enclosingAttr =
  if filterAttributes "sized" enclosingAttr <> [] then
    typeAddAttributes [AId("sized")] arrayType
  else
    arrayType

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
          let n = N.getNode p nextidx bt' a in
          TPtr (bt', n.N.attr), i'
  end
  | TArray(bt, len, a) -> begin
      (* wes: we want a node for the array, just like we have a node for
       * each pointer *)
      let sized = filterAttributes "sized" a <> [] in 
      match N.nodeOfAttrlist a with
        Some n -> TArray (bt, len, a), nextidx (* Already done *)
      | None -> 
          let bt', i' = doType bt p (nextidx + 1) in
          let n = N.getNode p nextidx bt' a in
          TArray (bt', len, n.N.attr), i'
  end
          
  | TComp comp -> 
      if H.mem doneComposites comp.ckey then
        t, nextidx
      else begin
        H.add doneComposites comp.ckey true; (* before we do the fields *)
        List.iter 
          (fun f -> 
            let fftype = addArraySizedAttribute f.ftype f.fattr in 
            let t', i' = doType fftype (N.PField f) 0 in
            f.ftype <- t') comp.cfields;
        t, nextidx
      end
        
    (* Strip the type names so that we have less sharing of nodes. However, 
     * we do not need to do it if the named type is a structure, and we get 
     * nicer looking programs *)
  | TNamed (n, bt, a) -> 
      let iscomp = match bt with TComp comp -> true | _ -> false in
      if iscomp then
        let t', _ = doType bt (N.PType n) 0 in
        TNamed (n, t', a), nextidx
      else
        let t', nextidx' = doType bt p nextidx in
        t', nextidx'
        
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
      let i' = 
        List.fold_left 
          (fun nidx arg -> 
            let t', i' = doType arg.vtype p nidx in
            arg.vtype <- t'; (* Can change in place because we shall copy the 
                              * varinfo for polymorphic functions *)
            i') i0 args in
      let newtp = TFun(restyp', args, isva, a) in
      newtp, i'

          

(* For each node corresponding to a struct or union or array type we will 
 * create successor node corresponding to various offsets. We cache these 
 * nodes indexed by the start node id and the name of the field. In the 
 * particular case of an array, we use the field name "@field" to refer to 
 * the first element *)
let offsetNodes : (int * string, N.node) H.t = H.create 111

(* Create a new offset node *)
let newOffsetNode (n: N.node)  (fname: string) 
                  (btype: typ) (battr: attribute list) = 
  let next = N.newNode (N.POffset(n.N.id, fname)) 0 btype battr in
  (* Add edges between n and next *)
  (match unrollType n.N.btype with
    TComp c when not c.cstruct -> (* A union *)
      N.addEdge n next N.ECast (-1);
      N.addEdge n next N.ESafe (-1)

  | TArray _ -> (* An index *)
      N.addEdge n next N.EIndex (-1)

  | TComp c when c.cstruct -> (* A struct *)
      N.addEdge n next N.ESafe (-1)

  | _ -> E.s (E.bug "Unexpected offset"));
  next

(* Create a field successor *)
let fieldOfNode (n: N.node) (fi: fieldinfo) = 
  try
    H.find offsetNodes (n.N.id, fi.fname)
  with Not_found -> 
    newOffsetNode n fi.fname fi.ftype []

let startOfNode (n: N.node) = 
  try
    H.find offsetNodes (n.N.id, "@first")
  with Not_found -> begin
    match unrollType n.N.btype with
      TArray (bt, _, _) ->
        let next = N.newNode (N.POffset(n.N.id, "@first")) 0 bt [] in
        N.addEdge n next N.EIndex (-1);
        next
    | _ -> n (* It is a function *)
  end
    

(* Compute the sign of an expression. Extend this to a real constant folding 
 * + the sign rule  *)
type sign = SPos | SNeg | SAny | SLiteral of int

let rec signOf = function
    Const(CInt(n, _, _)) -> SLiteral n
  | Const(CChr c) -> SLiteral (Char.code c)
  | SizeOf _ -> SPos (* We do not compute it now *)
  | UnOp (Neg, e, _) -> begin
      match signOf e with
        SPos -> SNeg
      | SLiteral n -> SLiteral (- n)
      | SNeg -> SNeg
      | _ -> SAny
  end
  | UnOp (LNot, e, _) -> SPos
  | BinOp (PlusA, e1, e2, _) -> begin
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
  | BinOp (MinusA, e1, e2, _) -> begin
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
  let vi_vtype = addArraySizedAttribute vi.vtype vi.vattr in 
  (* Do the type of the variable. Start the index at 1 *)
  let t', _ = doType vi_vtype place 1 in
  vi.vtype <- t';
(*  ignore (E.log "Did varinfo: %s. T=%a\n" vi.vname
            d_plaintype vi.vtype); *)
  (* Associate a node with the variable itself. Use index = 0 *)
  let n = N.getNode place 0 vi.vtype vi.vattr in
  (* Add this to the variable attributes *)
  vi.vattr <- n.N.attr
    
(* Do an expression. Return an expression, a type and a node. The node is 
 * only meaningful if the type is a TPtr _. In that case the node is also 
 * refered to from the attributes of TPtr  *)
let rec doExp (e: exp) = 
  match e with 
    Lval lv -> 
      let lv', lvn = doLvalue lv false in
      Lval lv', lvn.N.btype, nodeOfType lvn.N.btype

  | AddrOf (lv) -> 
      let lv', lvn = doLvalue lv false in
      AddrOf (lv'), TPtr(lvn.N.btype, lvn.N.attr), lvn

  | StartOf lv -> 
      let lv', lvn = doLvalue lv false in
      let next = startOfNode lvn in
      StartOf lv', TPtr(next.N.btype, next.N.attr), next

  | UnOp (uo, e, tres) -> (* tres is an arithmetic type *)
      UnOp(uo, doExpAndCast e tres, tres), tres, N.dummyNode

  | SizeOf (t) ->
      let t', _ = doType t (N.anonPlace()) 0 in
      SizeOf (t'), uintType, N.dummyNode

        (* arithmetic binop *)
  | BinOp (((PlusA|MinusA|Mult|Div|Mod|Shiftlt|Shiftrt|
    Lt|Gt|Le|Ge|Eq|Ne|BAnd|BXor|BOr|LtP|GtP|LeP|GeP|EqP|NeP|MinusPP) as bop), 
           e1, e2, tres) -> 
             BinOp(bop, doExpAndCast e1 tres,
                   doExpAndCast e2 tres, tres), tres, N.dummyNode
       (* pointer arithmetic *)
  | BinOp (((PlusPI|MinusPI|IndexPI) as bop), e1, e2, tres) -> 
      let e1', e1t, e1n = doExp e1 in
      let sign = 
        signOf 
          (match bop with PlusPI|IndexPI -> e2 | _ -> UnOp(Neg, e2, intType)) 
      in
      (match sign with
        SLiteral 0 -> ()
      | SPos -> e1n.N.posarith <- true
      | SLiteral n when n > 0 -> e1n.N.posarith <- true
      | _ -> 
          if bop = IndexPI then (*  Was created from p[e] *)
             e1n.N.posarith <- true
          else 
            e1n.N.arith <- true);
      if sign = SLiteral 0 then
          e1', e1t, e1n
        else
          BinOp (bop, e1', doExpAndCast e2 intType, e1t), e1t, e1n
      
      
  | CastE (newt, e) -> 
      let newt', _ = doType newt (N.anonPlace ()) 0 in
      CastE (newt', doExpAndCast e newt'), newt', nodeOfType newt'

  | Const (CStr s) as e -> 
      (* Add a cast in front of all strings. This way we have a place where 
       * to attach a node *)
      let newt', _ = doType charPtrType (N.anonPlace ()) 0 in
      CastE (newt', e), newt', nodeOfType newt'

  | Compound (t, initl) -> 
      let t', _ = doType t (N.anonPlace ()) 0 in
        (* Construct a new initializer list *)
      let doOneInit (off: offset) (ei: exp) (tei: typ) acc = 
        (None, doExpAndCast ei tei) :: acc
      in
      let newinitl = List.rev (foldLeftCompound doOneInit t' initl []) in
      Compound (t', newinitl), t', nodeOfType t'
      
  | _ -> (e, typeOf e, N.dummyNode)


(* Do an lvalue. We assume conservatively that this is for the purpose of 
 * taking its address. Return a modifed lvalue and a node that stands for & 
 * lval. Just ignore the node and get its base type if you do not want to 
 * take the address of. *)
and doLvalue ((base, off) : lval) (iswrite: bool) : lval * N.node = 
  let base', startNode = 
    match base with 
      Var vi -> begin 
        (* ignore (E.log "doLval (before): %s: T=%a\n"
                  vi.vname d_plaintype vi.vtype); *)
        doVarinfo vi; (* It was done when the variable was declared !!! *)
        let vn = 
          match N.nodeOfAttrlist vi.vattr with Some n -> n | _ -> N.dummyNode
        in
        (* Now grab the node for it *)
        (* ignore (E.log "doLval: %s: T=%a (ND=%d)\n"
                  vi.vname d_plaintype vi.vtype vn.N.id); *)
        base, vn
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
  | Index(e, resto) -> begin
      let nextn = startOfNode n in
      nextn.N.posarith <- true;
      let newo, newn = doOffset resto nextn in
      let e', et, _ = doExp e in
      Index(e', newo), newn
  end


  
(* Now model an assignment of a processed expression into a type *)
and expToType (e,et,en) t (callid: int) = 
  let rec isZero = function
      Const(CInt(0, _, _)) -> true
    | CastE(_, e) -> isZero e
    | _ -> false
  in
  let isString = function
      Const(CStr(_)) -> true
    | _ -> false
  in
  let etn = nodeOfType et in
  let tn  = nodeOfType t in
  (* ignore (E.log "expToType e=%a (NS=%d) -> TD=%a (ND=%d)\n"
            d_plainexp e etn.N.id d_plaintype t tn.N.id); *)
  match etn == N.dummyNode, tn == N.dummyNode with
    true, true -> e
  | false, true -> e (* Ignore casts of pointer to non-pointer *)
  | false, false -> 
      let edgetype = 
        if isZero e then begin tn.N.null <- true; N.ENull end else N.ECast in
      N.addEdge etn tn edgetype callid; 
      e
  | true, false -> 
      (* Cast of non-pointer to a pointer. Check for zero *)
      (if isZero e then
        tn.N.null <- true
      else if not (isString e) then 
        tn.N.intcast <- true
        );
      e
    
and doExpAndCast e t = 
  expToType (doExp e) t (-1)

and doExpAndCastCall e t callid = 
  expToType (doExp e) t callid



(* We will accumulate the marked globals in here *)
let theFile : global list ref = ref []

let debugInstantiate = false

let instantiatePolyFunc (vi: varinfo) : varinfo * bool =
  (* The args might be shared with other declarations and with formals for 
   * defined functions. Make shallow copies of all of them  *)
  let shallowCopyFunctionType t = 
    match unrollType t with
      TFun (rt, args, isva, fa) -> 
        TFun (rt, 
              List.map (fun a -> {a with vname = a.vname}) args,
              isva, fa)
    | _ -> E.s (E.bug "instantiating a non-function (%s)\n" vi.vname)
  in
  let old_name = vi.vname in
  if debugInstantiate then
    ignore (E.log "Instantiating function %s\n" old_name);
  let res, ispoly = 
    try
      let origtypref = H.find polyFunc vi.vname in
      if debugInstantiate then
        ignore (E.log "   poly function %s\n"
                  vi.vname);
      let origtype = (* Copy the type and remember it *)
        match !origtypref with
          Some t -> 
            if debugInstantiate then
              ignore (E.log "  Not the first time. Copying: %a\n"
                        d_plaintype t);
            shallowCopyFunctionType t
        | None -> 
            (* make a copy to return now *)
            let copiedtype = shallowCopyFunctionType vi.vtype in
            (* Make a copy to memoize and use to template new copies *)
            let copiedtype2 = shallowCopyFunctionType vi.vtype in
            if debugInstantiate then
              ignore (E.log "  The first time. Made template: %a\n"
                        d_plaintype copiedtype);
            origtypref := Some copiedtype2;
            copiedtype
      in
      let newvi = 
        {vi with vtype = origtype; (* Set it like this and doVarInfo 
                                      will fix it *)
                 vname = ("/*" ^ (string_of_int (!polyId + 1)) ^ "*/" ^ 
                          vi.vname)}  in
      incr polyId;
      newvi, true
    with Not_found -> 
      if debugInstantiate then
        ignore (E.log "  not polymorphic\n");
      vi, false    (* Not polymorphic *)
  in
  doVarinfo res;
  if debugInstantiate then begin
    ignore (E.log " After instantiatePoly: %s T=%a\n"
              res.vname d_plaintype res.vtype);
    (* Check the template, again *)
    try
      let origtypref = H.find polyFunc old_name in
      match !origtypref with
        Some t -> 
          ignore (E.log "  and the template is now: %a\n"
                    d_plaintype t)
      | None -> E.s (E.bug " there should be a template\n")
    with Not_found -> ()
  end;
  res, ispoly

(* possible printf arguments *)
type printfArgType = FormatInt | FormatDouble | FormatPointer

let d_printfArgType () at = begin
  match at with
    FormatInt -> text "int"
  | FormatDouble -> text "double"
  | FormatPointer -> text "pointer"
end

(* interpret a conversion specifier: the stuff that comes after the % 
 * in a printf format string *)
let rec parseConversionSpec f start = begin
  try 
  match Char.lowercase f.[start] with
    'e' | 'f' | 'g' |   (* double arg *)
    'a' ->              (* double arg *)
      FormatDouble, (start+1)
    | 'd' | 'i' |         (* signed int arg *)
    'o' | 'u' | 'x' |   (* unsigned int arg *)
    'c' |               (* unsigned char *)
    'p' ->              (* void pointer treated as int *)
      FormatInt, (start+1)
    | 's' -> FormatPointer, (start+1) (* char pointer *)
    | 'n' -> E.s (E.bug "Cannot handle 'n' character in format string [%s]" f)
    | _ -> parseConversionSpec f (start+1)
  with _ ->
    ignore (E.warn "Malformed format string [%s], assuming int arg at end" f) ;
    FormatInt, (start+1)
end

(* take apart a format string and return a list of int/pointer choices *)
let rec parseFormatString f start = begin
  try 
    let i = String.index_from f start '%' in
    if f.[i+1] = '%' then (* an escaped %, not a format char *) 
      parseFormatString f (i+2)
    else begin
      (* look after the % to see if they want an Int or a Pointer *)
      let t, next_start = parseConversionSpec f (i+1) in
      t :: (parseFormatString f next_start) (* look for another % *)
    end
  with _ -> []  (* no more % left in format string *)
end

(* remove casts *)
let removeCasts e = begin
  match e with 
    CastE(t,e') -> e'
  | _ -> e 
end

(* return the first few items of a list *)
let rec list_first l n = begin
  if n >= 0 then (List.hd l) :: (list_first (List.tl l) (n-1))
  else []
end

(* Handle printf-like functions *)
let isPrintf reso orig_func args (v : varinfo) = begin
  (* extract the first o+1 args from orig_func  *)
  let first_few_args orig_func o = begin
    match orig_func with 
      TFun(t,a,isv,attr) -> list_first a o
    | _ -> E.s (E.bug "isPrintf.first_few_args")
  end in 
  try 
    let o = Hashtbl.find printfFunc v.vname in begin
    let format_arg = removeCasts (List.nth args o) in 
    match format_arg with (* find the format string *)
      Const(CStr(f)) -> 
        let argTypeList = parseFormatString f 0 in 
        (*
        ignore (E.warn "Found printf [%s]@!Infer arg types: %a" f
          (docList (chr ',' ++ break) (d_printfArgType ())) argTypeList) ;
        *)
        (* OK, let's create a new wrapper for this printf-like function *)
        false
    | _ -> 
      ignore (E.warn "%s called with non-const format string %a" 
        v.vname d_exp format_arg) ; 
      false (* cannot handle non-constant format strings *)
    end
  with _ ->
    false (* we only handle declared printf-like functions *)
end

(* Do a statement *)
let rec doStmt (s: stmt) = 
  match s with 
    (Skip | Label _ | Case _ | Default | Break | Continue | Goto _) -> s
  | Sequence sl -> Sequence (List.map doStmt sl)
  | Loop s -> Loop (doStmt s)
  | IfThenElse (e, s1, s2, l) -> 
      IfThenElse (doExpAndCast e intType, doStmt s1, doStmt s2, l)
  | Switch (e, s, l) -> Switch (doExpAndCast e intType, doStmt s, l)
  | Return (None, _) -> s
  | Return (Some e, l) -> 
      Return (Some (doExpAndCast e !currentResultType), l)
  | Instr (Asm _, _) -> s
  | Instr (Set (lv, e), l) -> 
      let lv', lvn = doLvalue lv true in
      (* Now process the copy *)
(*      ignore (E.log "Setting lv=%a\n lvt=%a (ND=%d)" 
                d_plainlval lv d_plaintype (typeOfLval lv) lvn.N.id); *)
      let e' = doExpAndCast e lvn.N.btype in
      Instr (Set (lv', e'), l)

  | Instr (Call (reso, orig_func, args), l) -> 
      let func = (* check and see if it is polymorphic *)
        match orig_func with
          (Lval(Var(v),NoOffset)) -> 
            let is_printf = isPrintf reso orig_func args v in
            let newvi, ispoly = instantiatePolyFunc v in
            (* And add a declaration for it *)
            if ispoly then
              theFile := GDecl (newvi, lu) :: !theFile;
            (Lval(Var(newvi), NoOffset)) 
        | _ -> orig_func
      in
      let func', funct, funcn = doExp func in
      let (rt, formals, isva) = 
        match unrollType funct with
          TFun(rt, formals, isva, _) -> rt, formals, isva
        | _ -> E.s (E.bug "Call to a non-function")
      in
      incr callId; (* A new call id *)
      (* Now check the arguments *)
      let rec loopArgs formals args = 
        match formals, args with
          [], _ when (isva || args = []) -> args
        | fo :: formals, a :: args -> 
            let a' = doExpAndCastCall a fo.vtype !callId in
            a' :: loopArgs formals args
        | _, _ -> E.s (E.bug "Not enough arguments")
      in  begin
          (* Now check the return value*)
        match reso, unrollType rt with
          None, TVoid _ -> ()
        | Some _, TVoid _ -> 
            ignore (E.warn "Call of subroutine is assigned")
        | None, _ -> () (* "Call of function is not assigned" *)
        | Some destvi, _ -> 
            N.addEdge 
              (nodeOfType rt)
              (nodeOfType destvi.vtype) 
              N.ECast !callId  
      end;
      Instr (Call(reso, func', loopArgs formals args), l)
  
      
(* Now do the globals *)
let doGlobal (g: global) : global = 
  match g with
  | GPragma (a, _) as g -> begin
      (match a with
        ACons("boxpoly", [ AStr(s) ]) -> 
          ignore (E.log "Will treat %s as polymorphic\n" s); 
          H.add polyFunc s (ref None)
      | ACons("box", [AId("on")]) -> boxing := true
      | ACons("box", [AId("off")]) -> boxing := false
      | _ -> ());
      g
    end
  | _ -> 
      if not !boxing then g
      else match g with
      | GText _ | GAsm _ -> g

       (* Keep the typedefs only because they are convenient to define son 
        * struct tags. We won't use the TNamed  *)
      | GType (n, t, l) -> 
          let t', _ = doType t (N.PType n) 0 in
          GType (n, t', l)
            
      | GDecl (vi, _) -> 
      (* ignore (E.log "Found GDecl of %s. T=%a\n" vi.vname
                d_plaintype vi.vtype); *)
          if not (H.mem polyFunc vi.vname) then doVarinfo vi; 
          g
      | GVar (vi, init, l) -> 
          let init' = 
            match init with
              None -> None
            | Some i -> Some (doExpAndCast i vi.vtype)
          in
          GVar (vi, init', l)
      | GFun (fdec, l) -> 
          let newvi, ispoly = instantiatePolyFunc fdec.svar in
          if ispoly then
            fdec.svar <- newvi; (* Change the varinfo if the instantiation has 
                                   * changed it *)
          currentFunctionName := fdec.svar.vname;
          (* Go through the formals and copy their type and attributes from 
           * the type. Then restore the sharing  *)
          (match fdec.svar.vtype with
            TFun(rt, targs, isva, fa) -> 
              let rec scanFormals targs sformals = 
                match targs, sformals with
                  [], [] -> ()
                | ta :: targs, sf :: sformals -> 
                    sf.vtype <- ta.vtype;
                    sf.vattr <- ta.vattr;
                    scanFormals targs sformals
                | _ -> E.s (E.bug "scanFormals(%s) non-matching formal lists"
                              fdec.svar.vname)
              in
              scanFormals targs fdec.sformals;
          (* Restore the sharing by writing the type *)
              setFormals fdec fdec.sformals;
              currentResultType := rt
          | _ -> E.s (E.bug "Not a function")); 
          (* Do the other locals *)
          List.iter doVarinfo fdec.slocals;
          (* Do the body *)
          fdec.sbody <- doStmt fdec.sbody;
          g
      | GPragma _ -> g (* Should never be reached *)
      
      
(* Now do the file *)      
let markFile fl = 
  currentFileName := fl.fileName;
  boxing := true;
  E.hadErrors := false;
  H.clear polyFunc;
  (* Find the globals that are declared but not defined. They are part of 
   * the interface. *)
  let interfglobs : (int, varinfo) H.t = H.create 111 in
  let processDecls = function
      GDecl (vi, _) ->
        H.add interfglobs vi.vid vi
    | _ -> ()
  in
  (* Add all the declarations *)
  List.iter processDecls fl.globals;
  (* Take out the defined functions *)
  let processDefs = function
      GVar (vi, _, _) -> 
        H.remove interfglobs vi.vid
    | GFun (fdec, _) -> 
        H.remove interfglobs fdec.svar.vid
    | _ -> ()
  in
  List.iter processDefs fl.globals;

  (* See what functions we must make polymorphic *)
  if !N.allPoly || !N.externPoly then
    (* Go through the file once and find all declarations - definitions (for 
     * functions only) *)
    let processFunDecls glob = 
      let vio = 
        match glob with 
          GDecl (vi, _) when isFunctionType vi.vtype -> Some vi
        | GFun (fdec, _) -> Some fdec.svar
        | _ -> None
      in
      match vio with
        Some vi -> 
          if !N.allPoly || (!N.externPoly && 
                            H.mem interfglobs vi.vid) then
            H.add polyFunc vi.vname (ref None)
      | _ -> ()
    in
    List.iter processFunDecls fl.globals;
  else begin
    (* Otherwise we start with some defaults *)
    List.iter (fun s -> H.add polyFunc s (ref None)) 
      ["free"; "malloc"; "calloc"; "calloc_fseq"; "realloc"];
  end;
  (* initialize the default printf-like functions *)
  List.iter (fun (s,i) -> H.add printfFunc s i)
    [("printf",0) ; ("fprintf",1) ; ("sprintf",1) ; ("snprintf",2)] ;
  theFile := [];
  List.iter (fun g -> let g' = doGlobal g in 
                      theFile := g' :: !theFile) fl.globals;
  (* Now do the globinit *)
  let newglobinit = 
    match fl.globinit with
      None -> None
    | Some g -> begin
        match doGlobal (GFun(g, locUnknown)) with
          GFun (g', _) -> Some g'
        | _ -> E.s (E.bug "markptr: globinit")
    end
  in
  let newglobals = List.rev !theFile in
        
     
  (* Now go through the types of interfglobs and mark all nodes that are part 
   * of the interface *)
  H.iter
    (fun id vi -> 
      ignore 
        (existsType 
           (fun t -> 
             match N.nodeOfAttrlist (typeAttrs t) with
               Some n -> n.N.interface <- true; ExistsMaybe
             | _ -> ExistsMaybe)
           vi.vtype))
    interfglobs;
    
  ignore (E.log "Markptr: %s\n"
            (if !E.hadErrors then "Error" else "Success"));
  let newfile = {fl with globals = newglobals; globinit = newglobinit} in
  if !Util.doCheck then
    Check.checkFile [] newfile;
  newfile

        


(* A special file printer *)
let printFile (c: out_channel) fl = 
  Cil.setCustomPrint (N.ptrAttrCustom true)
    (fun fl ->
      let opi = !printIndent in
      printIndent := false;
      Stats.time "printMarkedfile" (Cil.printFile c) fl;
      output_string c "#if 0\n/* Now the graph */\n";
      (* N.gc ();   *)
      (* N.simplify ();   *)
      (* N.printGraph c; 
      output_string c "/* End of graph */\n"; *)
      output_string c "/* Now the solved graph (simplesolve) */\n";
      Stats.time "simple solver" Simplesolve.solve N.idNode ; 
      Stats.time "printgraph" N.printGraph c;
      printIndent := opi;
      output_string c "/* End of solved graph*/\n#endif\n";
      ) 
    fl ;
  (* Cil.setCustomPrint (N.ptrAttrCustom false)
    (fun fl -> Cil.printFile c fl) fl; *)
  ()

