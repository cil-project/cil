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

let useGenerics = ref false
let genericId   = ref 0

let callId = ref (-1)  (* Each call site gets a new ID *)

let polyId = ref (-1) 

let matchSuffix (lookingfor: string) (lookin: string) = 
  let inl = String.length lookin in
  let forl = String.length lookingfor in
  inl >= forl && String.sub lookin (inl - forl) forl = lookingfor

(* Match two names, ignoring the polymorphic prefix *)
let matchPolyName (lookingfor: string) (lookin: string) = 
  let inl = String.length lookin in
  if inl = 0 then false else
  if String.get lookin 0 = '/' then
    let rec loop i = (* Search for the second / *)
      if i >= inl - 1 then false else 
      if String.get lookin i = '/' then 
        String.sub lookin (i + 1) (inl - i - 1) = lookingfor
      else loop (i + 1)
    in
    loop 1
  else lookin = lookingfor

(* weimer: utility function to ease the transition between our flag formats *)
let setPosArith n = begin
  n.N.posarith <- true ; N.setFlag n N.pkPosArith ;
end
let setArith n = begin
  n.N.arith <- true ; N.setFlag n N.pkArith ;
end
let setNull n = begin
  n.N.null <- true ; N.setFlag n N.pkNull ;
end
let setUpdated n = begin
  n.N.updated <- true ; N.setFlag n N.pkUpdated ;
end
let setIntCast n = begin
  n.N.intcast <- true ; N.setFlag n N.pkIntCast ;
end
let setInterface n = begin
  n.N.interface <- true ; N.setFlag n N.pkInterface ;
end

(* A number of functions will be treated polymorphically. In that case store 
 * their original type. When we process the type of a polymorphic function we 
 * first store here the original type.  *)
let polyFunc : (string, typ option ref) H.t = H.create 7

(* Remember the bodies of polymorphic functions *)
let polyBodies : (string, fundec) H.t = H.create 7

(* all of the printf-like format string functions. The integer is the
 * argument number of the format string. *)
let printfFunc : (string, int ) H.t = H.create 15

(* We keep track of all functions that are decalred or defined *)
type funinfo = Declared of varinfo | Defined of fundec
let allFunctions: (string, funinfo) H.t = H.create 113

(* We keep track of the models to use *)
let boxModels: (string, fundec) H.t = H.create 15 (* Map the name of the 
                                                    * modelled function to 
                                                    * the model *)


(* We need a function that copies a CIL function. *)
let copyFunction (f: fundec) (newname: string) = 
  visitCilFunction (new copyFunctionVisitor(newname)) f
  

(* We keep track of a number of type that we should not unroll *)
let dontUnrollTypes : (string, bool) H.t = H.create 19


(* if some enclosing context [like the attributes for a field] says that
 * this array should be sized ... we do not want to forget it! *)
let addArraySizedAttribute arrayType enclosingAttr =
  if filterAttributes "sized" enclosingAttr <> [] then
    typeAddAttributes [Attr("sized",[])] arrayType
  else
    if hasAttribute "safeunion" enclosingAttr then
      typeAddAttributes [Attr("safeunion",[])] arrayType
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



(* We will accumulate the marked globals in here *)
let theFile: global list ref = ref []
    

(* Pass also the place and the next index within the place. Returns the 
 * modified type and the next ununsed index *)
let rec doType (t: typ) (p: N.place) 
               (nextidx: int) : typ * int = 
  match t with 
    (TVoid _ | TInt _ | TFloat _ | TEnum _ ) -> t, nextidx
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
          
  | TComp _ -> t, nextidx (* A forward reference, leave alone *)

    (* Strip the type names so that we have less sharing of nodes. However, 
     * we do not need to do it if the named type is a structure, and we get 
     * nicer looking programs. We also don't do it for base types *)
  | TNamed (n, bt, a) -> 
      let mustunroll = 
        (not (H.mem dontUnrollTypes n)) &&
        (match bt with 
        | TPtr _ -> true
        | TArray _ -> true
        | _ -> false)
      in
      if not mustunroll then
        let t', _ = doType bt (N.PType n) 0 in
        TNamed (n, t', a), nextidx
      else
        let t', nextidx' = doType bt p nextidx in
        t', nextidx'
        
  | TFun (restyp, args, isva, a) -> 
      let restyp', i0 = doType restyp p nextidx in
      let i' = 
        List.fold_left 
          (fun nidx arg -> 
            let t', i' = doType arg.vtype p nidx in
            arg.vtype <- t'; (* Can change in place because we shall copy the 
                              * varinfo for polymorphic functions *)
            i') i0 args 
      in
      (* See if it is a printf function *)
      (try
        if isva then 
          match p with 
            N.PGlob s -> begin
              let formatidx = H.find printfFunc s in
              let formatarg = List.nth args formatidx in
              let nd = nodeOfType formatarg.vtype in
              if nd == N.dummyNode then
                ignore (E.log "no node found\n")
              else begin
                nd.N.kind <- N.ROString; 
                nd.N.why_kind <- N.PrintfArg
              end
            end
          | _ -> ()
      with _ -> ());
      let newtp = TFun(restyp', args, isva, a) in
      newtp, i'

          

(* For each node corresponding to a struct or union or array type we will 
 * create successor node corresponding to various offsets. In the 
 * particular case of an array, we use the field name "@field" to refer to 
 * the first element *)
(* Create a new offset node *)
let newOffsetNode (n: N.node)  (fname: string) 
                  (btype: typ) (battr: attribute list) = 
  let mkNext () = N.newNode (N.POffset(n.N.id, fname)) 0 btype battr in
  (* ignore (E.log "newOffsetNode: n=%d, fname=%s\n" n.N.id fname); *)
  (* Add edges between n and next *)
  let next = 
    match unrollType n.N.btype with
      TComp (c, _) when not c.cstruct -> (* A union *)
        let next = mkNext () in
        N.addEdge n next N.ECast (-1);
        N.addEdge n next N.ESafe (-1);
        next
          
    | TArray (bt, l, a) -> (* An index *)
      (* Maybe the array already has a node *)
        let next = 
          match N.nodeOfAttrlist a with
            Some oldn -> 
              (* ignore (E.log "Reusing node %d\n" oldn.N.id); *)
              oldn
          | _ -> 
              (* ignore (E.log "Creating a node for array\n"); *)
              mkNext () (* Shouldn't there always be a node here ? *)
        in
        N.addEdge n next N.EIndex (-1);
        next
          
    | TComp (c, _) when c.cstruct -> (* A struct *)
        let next = mkNext () in
        N.addEdge n next N.ESafe (-1);
        next
          
    | _ -> E.s (bug "Unexpected offset")
  in
  next

(* Create a field successor *)
let fieldOfNode (n: N.node) (fi: fieldinfo) : N.node =
  newOffsetNode n fi.fname fi.ftype []

let startOfNode (n: N.node) : N.node =
  match unrollType n.N.btype with
    TArray (bt, _, a) -> newOffsetNode n "@first" bt a
  | _ -> n (* It is a function *)
  



(* Compute the sign of an expression. Extend this to a real constant folding 
 * + the sign rule  *)
type sign = SPos | SNeg | SAny | SLiteral of int64

let rec signOf = function
    Const(CInt64(n, _, _)) -> SLiteral n
  | Const(CChr c) -> SLiteral (Int64.of_int (Char.code c))
  | SizeOf _ -> SPos (* We do not compute it now *)
  | UnOp (Neg, e, _) -> begin
      match signOf e with
        SPos -> SNeg
      | SLiteral n -> SLiteral (Int64.neg n)
      | SNeg -> SNeg
      | _ -> SAny
  end
  | UnOp (LNot, e, _) -> SPos
  | BinOp (PlusA, e1, e2, _) -> begin
      match signOf e1, signOf e2 with
        SPos, SPos -> SPos
      | SLiteral n, SPos when n >= Int64.zero -> SPos
      | SPos, SLiteral n when n >= Int64.zero -> SPos
      | SLiteral n1, SLiteral n2 -> SLiteral (Int64.add n1 n2)
      | SNeg, SNeg -> SNeg
      | SLiteral n, SNeg when n <= Int64.zero -> SNeg
      | SNeg, SLiteral n when n <= Int64.zero -> SNeg
      | _ -> SAny
  end
  | BinOp (MinusA, e1, e2, _) -> begin
      match signOf e1, signOf e2 with
        SPos, SNeg -> SPos
      | SLiteral n, SNeg when n >= Int64.zero -> SPos
      | SPos, SLiteral n when n <= Int64.zero -> SPos
      | SLiteral n1, SLiteral n2 -> SLiteral (Int64.sub n1 n2)
      | SNeg, SPos -> SNeg
      | SLiteral n, SPos when n <= Int64.zero -> SNeg
      | SNeg, SLiteral n when n >= Int64.zero -> SNeg
      | _ -> SAny
  end
  | _ -> SAny

(*********************** VARARG ********************************)

(* Keep track for each function name whether it is a vararg, and the 
 * alternatives, with a field name, a type and a type signature *)
let varargs: (string, (string * typ * typsig) list) H.t = H.create 15

(* Compute the signature of a type for comparing arguments *)
let argumentTypeSig (t: typ) = 
  let argumentPromotion (t : typ) : typ =
    match unrollType t with
      (* We assume that an IInt can hold even an IUShort *)
      TInt ((IShort|IUShort|IChar|ISChar|IUChar), a) -> TInt(IInt, a)
    | TInt _ -> t
          (* Treat floats as compatible with integers *)
    | TFloat (FFloat, a) -> TInt (IInt, a)
    | TEnum (_, a) -> TInt(IInt, a)
    | t -> t
  in
  (* Ignore all attributes *)
  typeSigWithAttrs (fun x -> []) (argumentPromotion t)

let prepareVararg (funname: string) (t: typ) : unit  = 
  let kinds = 
    match unrollType t with
      TComp (ci, _) when not ci.cstruct -> 
        List.map (fun fi -> fi.fname, fi.ftype, argumentTypeSig fi.ftype)
          ci.cfields
    | t' -> [("anon", t', argumentTypeSig t')]
  in
  (* Make sure that no two types are compatible *)
  let _ = 
    List.fold_left
      (fun prev ((thisn, thist, thiss) as this) -> 
        List.iter
          (fun (pn, pt, ps) -> 
            if thiss = ps then 
              E.s (error "Vararg type %a has compatible fields %s and %s\n"
                     d_type t pn thisn)) prev;
        this :: prev)
      []
      kinds
  in
  ignore (E.log "Will treat %s as a vararg function\n" funname);
  H.add varargs funname kinds


(* Prepare the argument in a call to a vararg function *)
let prepareVarargArguments
    (func: exp) 
    (args: exp list) : stmt list * exp list = 
  match func with 
    Lval(Var fvi, NoOffset) -> begin
      try
        let argkinds = H.find varargs fvi.vname in
        let (_, indices, args') = 
          List.fold_right
            (fun a (arg_idx, indices, args) -> 
              let t = typeOf a in
              let ts = argumentTypeSig t in
              (* Search for a compatible type in the kinds *)
              let rec loop (idx: int) = function
                  [] -> E.s (unimp "Argument %d does not match any expected type for vararg function %s" arg_idx fvi.vname)
                | (kn, kt, ks) :: restk when ks = ts -> 
                    (* We found a match *)
                    idx, doCastT a t kt
                | _ :: restk -> loop (idx + 1) restk
              in
              let k_idx, a' = loop 0 argkinds in
              (arg_idx - 1, k_idx :: indices, a' :: args))
            args
            (List.length args, [], [])
        in
        ignore (E.log "Indices are: [%a]\n"
                  (docList (chr ',') num) indices);
        [], args'
      with Not_found -> 
        E.s (E.bug "Call to vararg function %s without a descriptor" 
               fvi.vname)
    end
  | _ -> E.s (unimp "We do not handle pointers to varargs")
        


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
  (* Add this to the variable attributes. Note that this node might have been 
   * created earlier. Merge the attributes and make sure we get the _ptrnode 
   * attribute  *)
  vi.vattr <- addAttributes vi.vattr n.N.attr;
(*  ignore (E.log "varinfo: T=%a. A=%a\n" 
            d_plaintype vi.vtype (d_attrlist true) vi.vattr) *)
  ()

(* Do an expression. Return an expression, a type and a node. The node is 
 * only meaningful if the type is a TPtr _. In that case the node is also 
 * refered to from the attributes of TPtr. Otherwise the node is N.dummyNode
  *)
let rec doExp (e: exp) : exp * typ * N.node= 
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

  | SizeOfE (e) -> 
      let e', et', en' = doExp e in
      SizeOfE(e'), uintType, N.dummyNode

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
        SLiteral z when z = Int64.zero -> ()
      | SPos -> setPosArith e1n

      | SLiteral n when n > Int64.zero -> setPosArith e1n
      | _ -> 
          if bop = IndexPI then (*  Was created from p[e] *)
             setPosArith e1n
          else 
             setArith e1n);
      if sign = SLiteral Int64.zero then
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

  | _ -> (e, typeOf e, N.dummyNode)


(* Do initializers. All the initliazers at this point better be non-pointers *)
and doInit (i: init) : init * typ = 
  match i with 
  | SingleInit e -> 
      let e', t, n = doExp e in
      if n != N.dummyNode then 
        E.s (bug "Found pointer initializer: %a\n" d_init i);
      SingleInit e', t
          
  | CompoundInit (t, initl) -> 
      let t', _ = doType t (N.anonPlace ()) 0 in
      if nodeOfType t' != N.dummyNode then
        E.s (bug "found pointer initializer: %a\n" d_init i);
        (* Construct a new initializer list *)
      let doOneInit (off: offset) (ei: init) (tei: typ) acc = 
        let ei', _ = doInit ei in
        ei' :: acc
      in
      let newinitl = List.rev (foldLeftCompound doOneInit t' initl []) in
      CompoundInit (t', newinitl), t'

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
(*        ignore (E.log "doLval: %s: T=%a (ND=%d)\n"
                  vi.vname d_plaintype vi.vtype vn.N.id); *)
        base, vn
      end
    | Mem e -> 
        let e', et, ne = doExp e in
        if iswrite then
          setUpdated ne ;
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
      setPosArith nextn ;
      let newo, newn = doOffset resto nextn in
      let e', et, _ = doExp e in
      Index(e', newo), newn
  end


  
(* Now model an assignment of a processed expression into a type *)
and expToType (e,et,en) t (callid: int) (polycast: bool) : exp = 
  let etn = nodeOfType et in
  let tn  = nodeOfType t in
(*  ignore (E.log "expToType e=%a (NS=%d) -> TD=%a (ND=%d)\n"
            d_plainexp e etn.N.id d_plaintype t tn.N.id); *)
  match etn == N.dummyNode, tn == N.dummyNode with
    true, true -> e (* scalar -> scalar *)
  | false, true -> e (* Ignore casts of pointer to non-pointer *)
  | false, false -> (* pointer to pointer *)
      if isZero e then begin
        setNull tn ;
        N.addEdge etn tn N.ENull callid;
        e
      end else 
        let addCastEdge (e: exp) (etn: N.node) (desttn : N.node) 
                        (callid: int) : exp = 
           (* See if it is a cast to or from void * *)
          let isvoidstar, tovoid = 
            match etn.N.btype, desttn.N.btype with
              TVoid _, TVoid _ -> false, false
            | TVoid _, _ -> true, false
            | _, TVoid _ -> true, true
            | _ -> false, false
          in
          if isvoidstar && !useGenerics then begin
(*            ignore (E.log "Found a generic: %a\n" d_exp e); *)
            (* Now create a new function *)
            let genericFun =   
              let fdec = emptyFunction 
                  ("generic" ^ (if tovoid then "put" else "get") ^ 
                   (string_of_int !genericId)) in
              incr genericId;
              let argx  = makeLocalVar fdec "x" (TPtr(etn.N.btype, 
                                                      etn.N.attr)) in
              fdec.svar.vtype <- TFun(TPtr(desttn.N.btype, desttn.N.attr), 
                                      [ argx ], false, []);
              fdec.svar.vstorage <- Extern;
              theFile := GDecl (fdec.svar,lu) :: !theFile;
              fdec
            in
            (* Now use the function instead of adding an edge *)
            e
          end else begin
(*            ignore (E.log "Setting %a : %a -> %d\n"
                      d_plainexp e d_plaintype et desttn.N.id); *)
            N.addEdge etn desttn 
              (if polycast then N.ECast (* PolyCast *) else N.ECast) callid; 
            e
          end
        in
        addCastEdge e etn tn callid

  | true, false -> (* scalar -> pointer *)
      (* Check for zero *)
      (if isZero e then
        setNull tn 
      else begin
        match e with
          Const(CStr(_)) -> () 
        | _ -> 
          setIntCast tn 
      end);
      e
    
and doExpAndCast e t = 
  (* Get rid of cascades of casts of 0 *)
  let e' = if isZero e then zero else e in
  expToType (doExp e') t (-1) false

and doExpAndCastCall e t callid ispolyarg = 
  (* Get rid of cascades of casts of 0 *)
  let e' = if isZero e then zero else e in
  expToType (doExp e') t callid ispolyarg


let debugInstantiate = false

(* Keep track of all instantiations that we did, so we can copy the bodies *)
let instantiations: (string * varinfo) list ref = ref []

(* To avoid going into in an infinite loop keep a track of the instantiations 
 * that are being done *)
let recursiveInstantiations: (string * varinfo) list ref = ref []

(* Take a look at the function and see if we must instantiate it. Return an 
 * instance and a boolean saying if the function ispolymorphic. *)
let instantiatePolyFunc (fvi: varinfo) : varinfo * bool = 
  (* The args might be shared with other declarations and with formals for 
   * defined functions. Make shallow copies of all of them  *)
  let dropNodeAttrs a = dropAttribute a (Attr("_ptrnode", [])) in
  let shallowCopyFunctionType t = 
    match unrollType t with
      TFun (rt, args, isva, fa) -> 
        TFun (rt, 
              List.map (fun a -> {a with vname = a.vname}) args,
              isva, fa)
    | _ -> E.s (bug "instantiating a non-function (%s)\n" fvi.vname)
  in
  (* Copy a type but drop the existing nodes *)
  let rec copyTypeNoNodes (t: typ) = 
    match t with
      TPtr(bt, a) -> TPtr(copyTypeNoNodes bt, dropNodeAttrs a)
    | t -> t
  in
  let old_name = fvi.vname in
  let newvi, ispoly = 
    match List.filter (fun (on, ovi) -> on = old_name) !recursiveInstantiations
    with 
    | (_, ovi) :: _  -> ovi, true (* Reuse the recursive instantiation *)
    | [] -> begin
        try
          if debugInstantiate then 
            ignore (E.log "trying instantiatePoly %s\n" old_name);
          let origtypref = H.find polyFunc fvi.vname in
          if debugInstantiate then
            ignore (E.log "Instantiating poly function %s\n" old_name);
          let origtype = (* Copy the type and remember it *)
            match !origtypref with
              Some t -> 
                if debugInstantiate then
                  ignore (E.log "  Not the first time. Copying: %a\n"
                            d_plaintype t);
                shallowCopyFunctionType t
            | None -> 
                (* make a copy to return now *)
                let copiedtype = shallowCopyFunctionType fvi.vtype in
                (* Make a copy to memoize and use to template new copies *)
                let copiedtype2 = shallowCopyFunctionType fvi.vtype in
                if debugInstantiate then
                  ignore (E.log "  The first time. Made template: %a\n"
                            d_plaintype copiedtype);
                origtypref := Some copiedtype2;
                copiedtype
          in
          let newvi = 
            {fvi with vtype = origtype; (* Set it like this and doVarInfo will 
                                       * fix it  *)
              vname = ("/*" ^ (string_of_int (!polyId + 1)) ^ "*/" ^ 
                    fvi.vname);
              vattr = dropNodeAttrs fvi.vattr }  in
          incr polyId;
          theFile := GDecl (newvi, locUnknown) :: !theFile;
          instantiations := (old_name, newvi) :: !instantiations;
          newvi, true
        with Not_found -> 
          fvi, false    (* Not polymorphic *)
    end
  in
  if debugInstantiate && ispoly then begin
    ignore (E.log " After instantiatePoly: %s T=%a\n"
              newvi.vname d_plaintype newvi.vtype);
  end;
  newvi, ispoly

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
      [FormatDouble], (start+1)
    | 'd' | 'i' |         (* signed int arg *)
    'o' | 'u' | 'x' |   (* unsigned int arg *)
    'c' |               (* unsigned char *)
    'p' ->              (* void pointer treated as int *)
      [FormatInt], (start+1)
    | '*' ->            (* integer *)
      let a,b = parseConversionSpec f (start+1) in
      FormatInt :: a, b 
    | 's' -> [FormatPointer], (start+1) (* char pointer *)
    | 'n' -> E.s (bug "Cannot handle 'n' character in format string [%s]" f)
    | _ -> parseConversionSpec f (start+1)
  with _ ->
    ignore (warn "Malformed format string [%s], assuming int arg at end" f) ;
    [FormatInt], (start+1)
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
      t @ (parseFormatString f next_start) (* look for another % *)
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
let isPrintf reso orig_func args = begin
  match orig_func with
    (Lval(Var(v),NoOffset)) -> begin try 
      let o = Hashtbl.find printfFunc v.vname in begin
        let format_arg = removeCasts (List.nth args o) in 
        let rec remove_casts l =
          match l with
            CastE(_,e) -> remove_casts e
          | _ -> l
        in 
        match remove_casts format_arg with (* find the format string *)
          Const(CStr(f)) -> 
            let argTypeList = parseFormatString f 0 in 
            let num_arg_types = List.length argTypeList in
            (* insert an explicit cast to the right type for every argument *)
            let num_args = List.length args in 
            let new_args = ref [] in 
            for i = 0 to num_args-1 do
              let this_arg = List.nth args i in 
              if i = 0 && v.vname = "sprintf" then begin
                let temp_type = TPtr((TInt(IChar,[])),[]) in
                let cast_arg,t,n = doExp (CastE(temp_type,this_arg)) in
                n.N.kind <- N.Safe ;
                ignore (warn "Call to sprintf. Ought to use snprintf\n");
                n.N.why_kind <- N.PrintfArg ;
                new_args := cast_arg :: !new_args;
              end else if i < o then begin 
                new_args := this_arg :: !new_args;
              end else if i = o then begin
                let temp_type = TPtr((TInt(IChar,[])),[]) in
                let cast_arg,t,n = doExp (CastE(temp_type,this_arg)) in
                n.N.kind <- N.ROString ;
                n.N.why_kind <- N.PrintfArg ;
                new_args := cast_arg :: !new_args;
              end else begin
                let temp_type, rostring = 
                  let arg_type = 
                    if i - (o + 1) >= num_arg_types then begin
                      ignore (warn "More arguments than the format specifies");
                      FormatInt
                    end else List.nth argTypeList (i-(o+1))
                  in
                  match arg_type with
                    FormatInt -> (TInt(IInt,[])), false
                  | FormatDouble -> (TFloat(FDouble,[])), false
                  | FormatPointer -> (TPtr((TInt(IChar,[])),[])), true
                in 
                let cast_arg,t,n = doExp (CastE(temp_type,this_arg)) in
                if rostring then begin
                  n.N.kind <- N.ROString ; n.N.why_kind <- N.PrintfArg 
                end;
                new_args := cast_arg :: !new_args;
              end
            done ;
            Some(List.rev !new_args)
        | _ -> 
            ignore (warn "%s called with non-const format string %a" 
                      v.vname d_exp format_arg) ; 
            None (* cannot handle non-constant format strings *)
      end
    with Not_found ->
      None (* we only handle declared printf-like functions *)
    end 
  | _ -> None
end

(* Some utility functions for decomposing a function call so that we can 
 * process them in a special way *)
type callArgDescr = 
    { caOrig: exp; (* The original actual, without the cast that was added to 
                    * match it with the formal type *)
      caOrigType: typ; (* Type of caOrig *)
      caOrigNode: N.node; (* The node corresponding to caOrigType, if TPtr *)
      caFormal: varinfo;  (* The formal varinfo *)
      caFormalType: typ; (* The type of the corresponding formal *)
      caFormalNode: N.node; (* The node corrsponding to caFormalNode,if TPtr *)
    } 

let decomposeCall 
    (reso: lval option)
    (f: exp)
    (args: exp list) : (* result *) callArgDescr * 
                       (* args *) callArgDescr list = 
  (* Fetch the function type *)
  let rt, formals = 
    match unrollType (typeOf f) with
      TFun (rt, formals, _, _) -> rt, formals
    | _ -> E.s (E.bug "decomposingCall to a non-function")
  in
  if List.length formals <> List.length args then 
    E.s (E.bug "decomposeCall: mismatch of argument list length");
  (* A function to split a pointer type into base type, attribute and 
  * node of attributes *)
  let splitPtrType (t: typ) = 
    match unrollType t with
      TPtr(bt, a) -> begin
        bt, a, 
        (match N.nodeOfAttrlist a with
          Some n -> n
        | None -> N.dummyNode)
      end
    | _ -> voidType, [], N.dummyNode
  in
  let argDescr = 
    List.map2
      (fun a f ->
        (* Get the types of the arguments. But be prepared to strip the top 
         * level cast since it could have been added by cabs2cil *)
        let stripCast (e: exp) = match e with CastE(_, e') -> e' | _ -> e in
        let orig = stripCast a in
        let origt = typeOf orig in
        let orig_bt, orig_a, orig_n = splitPtrType origt in
        (* So the same for the formal *)
        let formt = f.vtype in
        let form_bt, form_a, form_n = splitPtrType formt in
        { caOrig = orig; caOrigType = origt; caOrigNode = orig_n;
          caFormal = f; caFormalType = formt; caFormalNode = form_n })
      args formals
  in
  (* Now the return type *)
  let rt_bt, rt_a, rt_n = splitPtrType rt in
  let resDescr = 
    match reso, rt with 
      None, TVoid _ -> 
        { caOrig = zero; caOrigType = voidType; caOrigNode = N.dummyNode;
          caFormal = dummyFunDec.svar; 
          caFormalType = voidType; caFormalNode = N.dummyNode }
    | None, _ -> { caOrig = zero; caOrigType = voidType; 
                   caOrigNode = N.dummyNode;
                   caFormal = dummyFunDec.svar; 
                   caFormalType = rt; caFormalNode = rt_n }
    | Some _, TVoid _ -> E.s (E.bug "decomposeCall: assigned subroutine")
    | Some lv, _ -> 
        let origt = typeOfLval lv in
        let orig_bt, orig_a, orig_n = splitPtrType origt in
        { caOrig = Lval (lv); caOrigType = origt; 
          caOrigNode = orig_n; caFormal = dummyFunDec.svar; 
          caFormalType = rt; caFormalNode = rt_n }
  in
  resDescr, argDescr


let rec doBlock blk = 
  if hasAttribute "nobox" blk.battrs then 
    blk
  else 
    { bstmts = List.map doStmt blk.bstmts; battrs = blk.battrs }

and doStmt (s: stmt) : stmt = 
  (match s.skind with 
    Goto _ | Break _ | Continue _ -> ()
  | Return (None, _) -> ()
  | Return (Some e, l) -> 
      currentLoc := l;
      s.skind <- Return (Some (doExpAndCast e !currentResultType), l)
  | Instr il -> 
      s.skind <- Instr (List.map doInstr il)
  | Loop (b, l) -> 
      currentLoc := l;
      s.skind <- Loop (doBlock b, l)
  | Block b -> s.skind <- Block (doBlock b)
  | If(e, b1, b2, l) -> 
      currentLoc := l;
      s.skind <- If (doExpAndCast e intType, doBlock b1, doBlock b2, l)
  | Switch (e, b, cases, l) -> 
      currentLoc := l;
      s.skind <- Switch(doExpAndCast e intType, doBlock b, cases, l));
  s

and doInstr (i:instr) : instr = 
  match i with
  | Asm _ -> i
  | Set (lv, e,l) -> 
      currentLoc := l;
      let lv', lvn = doLvalue lv true in
      (* Now process the copy *)
(*      ignore (E.log "Setting lv=%a\n lvt=%a (ND=%d)" 
                d_plainlval lv d_plaintype (typeOfLval lv) lvn.N.id); *)
      let e' = doExpAndCast e lvn.N.btype in
      Set (lv', e', l)

  | Call (reso, orig_func, args, l) as i -> 
      currentLoc := l;
      incr callId; (* A new call id *)
(*      ignore (E.log "Call %a args: %a\n" 
                d_plainexp orig_func
                (docList (chr ',') (fun a -> d_plaintype () (typeOf a)))
                args); *)
      let func, ispoly = (* check and see if it is polymorphic *)
        match orig_func with
          (Lval(Var(v),NoOffset)) -> 
            let newvi, ispoly = instantiatePolyFunc v in
            doVarinfo newvi;
            if ispoly then begin
              Lval (Var newvi, NoOffset), true
            end else (* Not polymorphic *) orig_func, false
        | _ -> orig_func, false
      in
      (* Do the function as if we were to take its address *)
      let pfunc, pfunct, pfuncn = 
        match func with 
          Lval lv -> doExp (mkAddrOf lv)
        | _ -> E.s (unimp "Called function is not an lvalue")
      in
      (* Now fetch out the real function and its type *)
      let func' = Lval (mkMem pfunc NoOffset) in
      let funct =
        match pfunct with
          TPtr ((TFun _ as funct), _) -> funct
        | _ -> E.s (bug "Expected a function pointer here")
      in
      let (rt, formals, isva) = 
        match unrollType funct with
          TFun(rt, formals, isva, _) -> rt, formals, isva
        | _ -> E.s (bug "Call to a non-function")
      in
      let isprintf = isPrintf reso func args in
      let args' = 
        if isva then 
          match isprintf with
            Some args'  -> args'
          | None -> let _, x = prepareVarargArguments func' args in x
        else
          args
      in
      (* If the function has more actual arguments than formals then mark the 
       * function node as used without prototype *)
      if List.length args' <> List.length formals && not isva then begin
        (* Bark if it is polymorphic. No prototype + polymorphism (or 
         * allocation) do not work together *)
        if ispoly || isprintf != None then 
          E.s (error "Calling polymorphic (or allocation) function %a without proper prototype" d_exp func);
        pfuncn.N.noPrototype <- true;
      end;
      (* Now check the arguments *)
      let rec loopArgs formals args = 
        match formals, args with
          [], [] -> []
        | [], a :: args -> (* We ran out of formals. This is bad, so we make 
                            * sure to mark that the argument is used in a 
                            * function without prototypes *)
            (* Do the arguments because they might contain pointer types *)
            let a', _, an = doExp a in
            if an != N.dummyNode && not isva  then
              an.N.noPrototype <- true;
            a' :: loopArgs [] args

        | fo :: formals, a :: args -> 
            (* See if this is a polymorphic argument *)
            let ispolyarg = 
              match ispoly, unrollType fo.vtype with
                true, TPtr(TVoid _, _) -> true
              | _, _ -> false
            in
(*            ignore (E.log "Call arg %a: %a -> %s\n" 
                      d_exp a' d_plaintype (typeOf a') fo.vname); *)
            let a' = doExpAndCastCall a fo.vtype !callId ispolyarg in
            a' :: loopArgs formals args

        | _, _ -> E.s (E.unimp "Markptr: not enough arguments in call to %a"
                         d_exp orig_func)
      in  
      let polyRet = 
        match ispoly, unrollType rt with 
          true, TPtr(TVoid _, _) -> true
        | _, _ -> false
      in
      begin
          (* Now check the return value*)
        match reso, unrollType rt with
          None, TVoid _ -> ()
        | Some _, TVoid _ -> 
            ignore (warn "Call of subroutine is assigned")
        | None, _ -> () (* "Call of function is not assigned" *)
        | Some dest, _ -> begin
            (* Do the lvalue, just so that the type is done *)
            let dest', _ = doLvalue dest true in
            (* Add the cast. Make up a phony expression and a node so that we 
             * can call expToType. *)
            ignore (expToType (Const(CStr("a call return")),
                               rt, N.dummyNode) (typeOfLval dest') 
                      !callId polyRet)
	end 
      end;
      (* Take a look at a few special function *)
      (match func' with 
        Lval(Var v, NoOffset) -> begin
          match args' with
            [a] -> 
              if matchPolyName "__endof" v.vname then begin
                let n = nodeOfType (typeOf a) in
                if n == N.dummyNode then
                  E.s (error "Call to __endof on a non pointer");
                setPosArith n (* To make sure we have an end *)
              end else if matchPolyName "__startof" v.vname then begin
                let n = nodeOfType (typeOf a) in
                if n == N.dummyNode then
                  E.s (error "Call to __startof on a non pointer");
                setArith n (* To make sure we have a start and an end *)
              end 
          | _ -> ()
        end
      | _ -> ());
      Call(reso, func', loopArgs formals args', l)


let doFunctionBody (fdec: fundec) = 
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
        | _ -> E.s (bug "scanFormals(%s) non-matching formal lists"
                      fdec.svar.vname)
      in
      scanFormals targs fdec.sformals;
      (* Restore the sharing by writing the type *)
      setFormals fdec fdec.sformals;
      currentResultType := rt
  | _ -> E.s (bug "Not a function")); 
  (* Do the other locals *)
  List.iter doVarinfo fdec.slocals;
  (* Do the body *)
  fdec.sbody <- doBlock fdec.sbody

let disableModelCheck = ref false
  
(* Now do the globals *)
let doGlobal (g: global) : global = 
  match g with
  | GPragma (a, l) as g -> begin
      currentLoc := l;
      (match a with
        Attr("boxpoly", [ AStr(s) ]) -> 
          if not (H.mem polyFunc s) then begin
            ignore (E.log "Will treat %s as polymorphic\n" s); 
            H.add polyFunc s (ref None)
          end

      | Attr("boxalloc", AStr(s) :: _) -> 
          if not (H.mem polyFunc s) then begin
            ignore (E.log "Will treat %s as polymorphic\n" s); 
            H.add polyFunc s (ref None)
          end

      | Attr("boxprintf", AStr(s) :: AInt(id) :: []) -> 
          if not (H.mem printfFunc s) then begin
            ignore (E.log "Will treat %s as a printf function\n" s);
            H.add printfFunc s id
          end

      | Attr("box", [AId("on")]) -> boxing := true
      | Attr("box", [AId("off")]) -> boxing := false

      | Attr("boxvararg", [AStr s; ASizeOf t]) -> 
          prepareVararg s t

      | _ -> ());
      g
    end
  | _ -> begin
      if not !boxing then g
      else match g with
      | GText _ | GAsm _ | GEnumTag _ -> g

       (* Keep the typedefs only because they are convenient to define son 
        * struct tags. We won't use the TNamed  *)
      | GType (n, t, l) -> 
          currentLoc := l;
          let t', _ = doType t (N.PType n) 0 in
          GType (n, t', l)

      | GCompTag (comp, l) -> 
          currentLoc := l;
          List.iter 
            (fun f -> 
              let fftype = addArraySizedAttribute f.ftype f.fattr in 
              let t', i' = doType fftype (N.PField f) 0 in
              f.ftype <- t') comp.cfields;
          (* Maybe we must turn this composite type into a struct *)
          if not comp.cstruct &&
            hasAttribute "safeunion" comp.cattr then
            comp.cstruct <- true;
          GCompTag (comp, l)
  
      | GDecl (vi, l) -> 
          currentLoc := l;
          (* ignore (E.log "Found GDecl of %s. T=%a\n" vi.vname
                    d_plaintype vi.vtype); *)
          if not (H.mem polyFunc vi.vname) then doVarinfo vi; 
          (match vi.vtype with
            TFun _ -> 
              if not (H.mem allFunctions vi.vname) then
                H.add allFunctions vi.vname (Declared vi)
          | _ -> ());
          g

      | GVar (vi, init, l) -> 
          currentLoc := l;
          let init' = 
            match init with
              None -> None
            | Some i -> 
                let i', _ = doInit i in
                Some i'
          in
          GVar (vi, init', l)
            
      | GFun (fdec, l) -> 
          currentLoc := l;
          (* See if it is a model for anybody *)
          if not !disableModelCheck then begin
            H.add allFunctions fdec.svar.vname (Defined fdec);
            (* Scan the models *)
            let modelattrs = filterAttributes "boxmodel" fdec.svar.vattr in
            List.iter 
              (function 
                  Attr(_, [AStr fname]) -> 
                    ignore (E.log "Will use %s as a model for %s\n"
                              fdec.svar.vname fname);
                    H.add boxModels fname fdec;
                | _ -> ()) modelattrs;
          end;
          (* If it is polymorphic then remember it for later. *)
          if H.mem polyFunc fdec.svar.vname then begin
            H.add polyBodies fdec.svar.vname fdec;
            GText ("// Body of " ^ fdec.svar.vname ^ " used to be here\n")
          end else begin
            doVarinfo fdec.svar;
            doFunctionBody fdec;
            g
          end


      | GPragma _ -> g (* Should never be reached *)
  end

      
(* Now do the file *)      
let markFile fl = 
  currentFileName := fl.fileName;
  boxing := true;
  disableModelCheck := false;
  E.hadErrors := false;
  H.clear polyFunc;
  H.clear polyBodies;
  H.clear allFunctions;
  H.clear boxModels;
  H.clear dontUnrollTypes;
  H.clear varargs;
  instantiations := [];
  (* Some globals that are exported and must thus be considered part of the 
   * interface *)
  let exported : (string, bool) H.t = H.create 111 in
  (* Find the globals that are declared but not defined. They are part of 
   * the interface. *)
  let interfglobs : (int, varinfo) H.t = H.create 111 in
  let processDecls = function
      GDecl (vi, l) ->
        currentLoc := l;
        if not (H.mem interfglobs vi.vid) then 
          (* Do not add multiple times since then deleting does not remove 
           * all copies *)
          H.add interfglobs vi.vid vi
    | GPragma (Attr("boxexported", [AStr s]), _) ->
        H.add exported s true
    | GPragma (Attr("boxnounroll", [AStr s]), _) ->
        H.add dontUnrollTypes s true
        
    | _ -> ()
  in
  (* Add all the declarations *)
  List.iter processDecls fl.globals;
  (* Take out the defined functions *)
  let rec processDefs = function
      GVar (vi, _, _) -> processVarinfo vi
    | GFun (fdec, _) -> processVarinfo fdec.svar
    | _ -> ()
  and processVarinfo vi = 
    if H.mem exported vi.vname then begin
      if not (H.mem interfglobs vi.vid) then
        H.add interfglobs vi.vid vi
    end else
      H.remove interfglobs vi.vid
  in
  List.iter processDefs fl.globals;
(*  ignore (E.log "inferfglob:\n");
  H.iter (fun sid v -> ignore (E.log "%s: %d@!" v.vname sid)) interfglobs; *)

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
      ["free" ];
  end;
  theFile := [];
  List.iter (fun g -> let g' = doGlobal g in 
                      theFile := g' :: !theFile) fl.globals;

  (* Now we must create a body for all of the modelled functions. We know that 
   * they do not have a definition already *)
  disableModelCheck := true;
  (* Scan all of the declared functions and see if they have a model *)
  H.iter 
    (fun fname -> fun info -> 
      try
        let model : fundec = H.find boxModels fname in
        (* We have a model *)
        let modelled : varinfo = 
          match info with 
            Defined x -> 
              E.s (E.bug "Function %s is both defined and has models!\n" fname)
          | Declared x -> x
        in
        ignore (E.log "Creating a body for %s based on model %s\n"
                  modelled.vname model.svar.vname);
        (* Make the sformals *)
        let rt, sformals, va, l = 
          match modelled.vtype with
            TFun(rt, args, va, l) -> rt, args, va, l 
          | _ -> E.s (E.bug "Modelled function %s does not have a function type"
                        modelled.vname)
        in
        (* Go over the formals and make sure that we assign the right ids. 
         * Being in a type they might not have the right IDS. This is safe 
         * since we know that there is no definition already for this func.  *)
        let vid = ref 0 in (* for local ids *)
        List.iter (fun s -> s.vid <- !vid; incr vid) sformals;
        let modelledFun = { svar     = modelled;
                            sformals = sformals;
                            slocals  = [];
                            smaxid   = !vid - 1;
                            sinline  = false;
                            sbody    = mkBlock [] } in
        (* Now make the body *)
        let reslvo, reso = 
          match rt with 
            TVoid _ -> None, None
          | t -> 
              let tmp = makeTempVar modelledFun t in
              Some (var tmp), Some (Lval (var tmp))
        in
        let call = 
          mkStmtOneInstr
            (Call (reslvo, Lval (var model.svar),
                   List.map (fun a -> Lval (var a)) sformals,
                   locUnknown)) in
        let return = mkStmt (Return (reso, locUnknown)) in
        modelledFun.sbody <- mkBlock [call; return];
        (* Mark it as modelled *)
        modelled.vattr <- 
           addAttribute (Attr("modelledbody",[])) modelled.vattr;
        (* If it is polymorphic we postpone it *)
        if H.mem polyFunc modelled.vname then begin
          H.add polyBodies modelled.vname modelledFun;
          (* We also go through all of the instantiations and mark them as 
           * modelledbody so their bodies can be dropped. The instantiations 
           * that are created later during the copying of the polymorphic 
           * functions will get their modelledbody attribute from the one we 
           * set above *)
          List.iter 
            (fun (n, vi) -> 
              if matchPolyName modelled.vname n then begin
                vi.vattr <- 
                   addAttribute (Attr("modelledbody",[])) vi.vattr
              end)
            !instantiations;
        end else begin
          (* Just mark the body *)
          theFile := GText ("// Modeling body of " 
                            ^ modelled.vname ^ " based on model " ^
                            model.svar.vname) :: !theFile;
          let g' = doGlobal (GFun (modelledFun, locUnknown)) in
          theFile :=  g' :: !theFile;
        end
      with Not_found -> ())
    allFunctions;
  
  let allinstantiations : (string * varinfo) list ref = ref [] in

  (* Now we must process the polymorphic functions. While we do that we might 
   * create new polymorphic instantiations. Careful about infinite loops *)
  let rec loopInstantiations (recs: (string * varinfo) list) (* parents *) = 
    (* See what new instantiations we got *)
    let newinst = List.rev !instantiations in (* Do them in order *)
    instantiations := [];
    allinstantiations := !allinstantiations @ newinst;
    List.iter (depthFirst recs) newinst

  and depthFirst (recs: (string * varinfo) list) (* Parents *)
                 ((oldname, newvi) as thisone)
      : unit = 
    assert (!instantiations == []);
    let recs' = thisone :: recs in
    recursiveInstantiations := recs'; (* Set the parents for instantiatePoly *)
    (try 
      let body = H.find polyBodies oldname in
      if debugInstantiate then 
        ignore (E.log "Copying body of function %s\n" newvi.vname);
      let f' = copyFunction body newvi.vname in
      (* We must use newvi as the svar but we have to preserve the 
      * sharing with sformals *)
      setFunctionType f' newvi.vtype;
      newvi.vtype <- f'.svar.vtype;
      f'.svar <- newvi;
      let g' = doGlobal (GFun(f', locUnknown)) in
      theFile :=  g' :: !theFile;
    with Not_found -> ()); (* This one does not have a body, or a model *)
    loopInstantiations recs'
  in
  (* Now do the instantiations *)
  loopInstantiations [];

  (* There might be some polymorphic functions that were not used. Make sure 
   * we have an instance of those as well, except if the body is a model. *)
  H.iter 
    (fun oldname -> fun body ->
      if  filterAttributes "boxmodel" body.svar.vattr != [] 
       && List.filter (fun (on, _) -> on = oldname) !allinstantiations != [] 
      then
        let g' = doGlobal (GFun(body, locUnknown)) in
        theFile :=  g' :: !theFile)
    polyBodies;
  
  (* Now do the globinit *)
  let newglobinit = 
    match fl.globinit with
      None -> None
    | Some g -> begin
        match doGlobal (GFun(g, locUnknown)) with
          GFun (g', _) -> Some g'
        | _ -> E.s (bug "markptr: globinit")
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
               Some n -> setInterface n; ExistsMaybe
             | _ -> ExistsMaybe)
           vi.vtype))
    interfglobs;
    
  ignore (E.log "Markptr: %s\n"
            (if !E.hadErrors then "Error" else "Success"));
  let newfile = {fl with globals = newglobals; globinit = newglobinit} in
  if !Util.doCheck then
    Check.checkFile [] newfile;
  H.clear polyFunc;
  H.clear polyBodies;
  H.clear boxModels;
  H.clear allFunctions;
  H.clear dontUnrollTypes;
  H.clear varargs;
  recursiveInstantiations := [];
  instantiations := [];
  newfile

        
let solver = ref "simple"

(* A special file printer *)
let printFile (c: out_channel) fl = 
  Cil.setCustomPrintAttributeScope (N.ptrAttrCustom true)
    (fun fl ->
      (* AB: These flags are no longer used by Pretty *)
(*
      let o_noal = !noAligns in
      let o_nobr = !noBreaks in
      noAligns := true; noBreaks := true;
*)
      Stats.time "printMarkedfile" (Cil.printFile c) fl;
      output_string c "#if 0\n/* Now the graph */\n";
      (* N.gc ();   *)
      (* N.simplify ();   *)
      (* N.printGraph c; 
      output_string c "/* End of graph */\n"; *)
      output_string c "/* Now the solved graph (simplesolve) */\n";
      Stats.time "printgraph" N.printGraph c;
(*
      noAligns := o_noal; noBreaks := o_nobr;
*)
      output_string c "/* End of solved graph*/\n#endif\n";
      ) 
    fl ;
  Stats.time "graph stats" N.printGraphStats ();
  (* Cil.setCustomPrint (N.ptrAttrCustom false)
    (fun fl -> Cil.printFile c fl) fl; *)
  ()

