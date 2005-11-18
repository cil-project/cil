(*
 *
 * Copyright (c) 2004, 
 *  Jeremy Condit       <jcondit@cs.berkeley.edu>
 *  George C. Necula    <necula@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
open Cil
open Pretty
module E = Errormsg
module GA = GrowArray

let debug : bool ref = ref false
let verbose : bool ref = ref false
let suppress : bool ref = ref false

let curFunc : fundec ref = ref dummyFunDec
let curStmt : int ref = ref (-1)

(* Assign to each statement a unique ID. *)
let nextStmtId : int ref = ref 0
let assignID (s:stmt) : unit =
(*   E.log "sid = %d.  s = %a.\n" s.sid d_stmt s; *)
  assert (s.sid = -1); (* Make sure that no one else has assigned ID numbers *)
  s.sid <- !nextStmtId;
  incr nextStmtId;
  ()

(* Convert instruction lists into individual statements, and give each
  stmt a unique id. *)
let fixStmt (s:stmt) : unit =
  assignID s;
  match s.skind with 
    Instr [] -> ()
  | Instr [i] -> ()
  | Instr il -> (* Two or more instructions *)
      s.skind <- Block (mkBlock (List.map mkStmtOneInstr il));
      ()      
  | _ -> ()

(**************************************************************************)

type check =
    CNonNull of exp      (** e != 0 *)
  | CEq of exp * exp     (** e1 == e2 *)
  | CNotEq of exp * exp  (** e1 != e2,   e.g. e != hi *)
  | CPositive of exp     (** e > 0 *)
  | CBounds of exp * exp * exp * exp 
                         (** e1 <= e2+e3 <= e4.  For ptr arith *)
  | CCoerce of exp * exp * exp * exp * exp
                         (** e3 == 0 ||
                             e1 <= e2 <= e3 <= e4 <= e5 *)
(* Other checks will be needed, such as nullterm checks and checks for when
   part of one of the above checks can be proved statically. *)


(* A mapping from stmt ids to checks for that instruction. The list of checks 
   is stored in reverse order.  *)
let allChecks : check list GA.t = GA.make 200 (GA.Elem [])

(* Add a check for the current statement.
 * When more than one check is added for a statement, they will be executed in
 * the order that addCheck was called.
 * For example, when accessing "**e", call addCheck (CNonNull e)
 * first, then addCheck (CNonNull *e).  Then we'll be sure to check e 
 * before *e, which ensures that the *e check won't segfault. 
 *)
let addCheck (c:check) : unit =
  let id = !curStmt in
  if id < 0 then 
    E.s (bug "addCheck when sid = %d.\n" id);
  let otherChecks = GA.getg allChecks id in
  if not (List.mem c otherChecks) then
    GA.set allChecks id (c::otherChecks)

(* These aren't real variables.  In the output, they'll show up as
   __FILE__ and __LINE__, and gcc will see them as macros.  We use them
   for calling runtime check functions. *)
let fileToken : exp=
  let vi = makeGlobalVar "__FILE__" charPtrType in
  Lval (var vi)
let lineToken : exp=
  let vi = makeGlobalVar "__LINE__" intType in
  Lval (var vi)

let mkCheckFun (n: string) (numargs: int) : exp = 
  let fdec = emptyFunction n in
  let args = Util.list_init numargs (fun _ -> ("", voidPtrType, [])) in
  let args' = args @ [("file",charPtrType,[]); ("line",intType,[])] in
  fdec.svar.vtype <- TFun(TVoid [], Some args', false, []);
  fdec.svar.vstorage <- Static;
  Lval (var fdec.svar)
let cnonnull = mkCheckFun "CNonNull" 1
let ceq = mkCheckFun "CEq" 2
let cnoteq = mkCheckFun "CNotEq" 2
let cpositive = mkCheckFun "CPositive" 1
let cbounds = mkCheckFun "CBounds" 3
let ccoerce = mkCheckFun "CCoerce" 5

let checkToInstr (c:check) =
  let call f args = Call(None, f,
                         (* Append the file and line to the end of the args *)
                         args @ [fileToken; lineToken],
                         !currentLoc) 
  in
  match c with
    CNonNull (e) -> call cnonnull [e]
  | CEq (e1,e2) -> call ceq [e1;e2]
  | CNotEq (e1,e2) -> call cnoteq [e1;e2]
  | CPositive (e) -> call cpositive [e]
  | CBounds (b,p,off,e) -> let p' = BinOp(PlusPI, p, off, typeOf p) in
                           call cbounds [b;p';e]
  | CCoerce (e1,e2,e3,e4,e5) -> call ccoerce [e1;e2;e3;e4;e5]


let postPassVisitor = object (self)
  inherit nopCilVisitor

  (* Turn the check datastructure into explicit checks, so that they show up
     in the output. *)
  method vstmt s = 
    let postProcessStmt (s: stmt) : stmt =
      let checks = GA.getg allChecks s.sid in
      if checks <> [] then begin
        let checks' = List.rev checks in (* put them back in the right order *)
        let checks'' : instr list = List.map checkToInstr checks' in
        self#queueInstr checks''
      end;
      s
    in
    ChangeDoChildrenPost (s, postProcessStmt)

  (* Remove any "bounds" or "fancybounds" annotations. *)
  method vattr a =
    match a with
      Attr(("bounds" | "fancybounds" | "nullterm"), _) -> ChangeTo []
    | _ -> DoChildren

end


(**************************************************************************)

(* remember complicated bounds expressions *)
let boundsTable : (int, exp) Hashtbl.t = Hashtbl.create 13
let boundsTableCtr : int ref = ref 0

let addBoundsExp (e: exp) : int =
  incr boundsTableCtr;
  if !verbose then
    E.log "%a:   fancybounds(%d) = %a.\n" d_loc !currentLoc
      !boundsTableCtr d_exp e;
  Hashtbl.add boundsTable !boundsTableCtr e;
  !boundsTableCtr

let getBoundsExp (n: int) : exp =
  try
    Hashtbl.find boundsTable n
  with Not_found ->
    E.s (E.bug "couldn't look up expression in bounds table\n")

let rec getDeps (a: attrparam) : string list =
  match a with 
  | AInt k -> []
  | ASizeOf t -> []
  | ACons(name, []) -> [name]
  | ABinOp (_, e1, e2) -> (getDeps e1) @ (getDeps e2)
  | _ -> E.s (E.error "Cannot get dependencies for %a" d_attrparam a)

let rec depsOfAttrs (a: attributes) : string list = 
  let checkrest rest =
    if hasAttribute "bounds" rest ||
       hasAttribute "fancybounds" rest then
      E.s (error "Type has duplicate bounds attributes")
  in
  match a with
  | Attr ("bounds", [lo; hi]) :: rest ->
      checkrest rest;
      (getDeps lo) @ (getDeps hi)
  | Attr ("bounds", _) :: rest ->
      E.s (error "Illegal bounds annotations.")
  | Attr ("fancybounds", _) :: rest ->
      E.s (error "Can't get dependencies for fancybounds annotations.")
  | Attr _ :: rest -> 
      depsOfAttrs rest
  | [] -> 
      E.s (error "Missing bounds annotations.")

let depsOfType (t: typ) : string list =
  match t with
  | TPtr (_, a) -> depsOfAttrs a
  | _ -> []

(* Determine whether other variables/fields depend on a given name. *)
let hasExternalDeps (lv: lval) : bool =
  let hasDeps (n: string) (vars: (string * typ) list) : bool =
    List.fold_left
      (fun acc (name, t) ->
         acc || (name <> n && List.mem n (depsOfType t)))
      false
      vars
  in
  let lv', off = removeOffsetLval lv in
  match off with
  | NoOffset ->
      begin
        match fst lv with
        | Var vi when vi.vglob ->
            E.s (E.unimp "global variables unimplemented\n")
        | Var vi ->
            assert (not vi.vglob);
            let vars =
              List.map (fun vi -> vi.vname, vi.vtype)
                       (!curFunc.slocals @ !curFunc.sformals)
            in
            hasDeps vi.vname vars
        | Mem e ->
            false
      end
  | Field (fld, NoOffset) ->
      let vars =
        List.map (fun fld -> fld.fname, fld.ftype) fld.fcomp.cfields
      in
      hasDeps fld.fname vars
  | Index (_, NoOffset) -> E.s (E.unimp "index offsets unsupported\n")
  | _ -> E.s (E.bug "unexpected result from removeOffset\n")

(* mapping from variable/field names to expressions representing 
   the runtime value. *)
type context = (string * exp) list

let isPointer e: bool =
  isPointerType (typeOf e)

let isNullterm (t: typ) : bool =
  match t with
  | TPtr (_, a) -> hasAttribute "nullterm" a
  | _ -> E.s (E.error "Expected pointer type.")

(* Keyword in bounds attributes representing the current value *)
let thisKeyword = "__this"

(** The dependent types are expressed using attributes. We compile an 
 * attribute given a mapping from names to lvals.  Returns the names of
 * meta values that this annotation depends on, and the expression.
 *  
 * This is a helper for both fields and formals. *)
let compileAttribute 
  (ctx: context) (* Should include a mapping for thisKeyword *)
  (a: attrparam) 
  : string list * exp = 
  let rec compile (a: attrparam) = 
    match a with 
      AInt k -> [], integer k
    | ASizeOf t -> [], SizeOf t
    | ACons(name, []) -> begin
(*         let name' = if name = "__this" then this else name in *)
        try 
          let e = List.assoc name ctx in 
          [name], e
        with Not_found -> 
          E.s (E.error 
                 ("Cannot compile the dependency %a: " ^^
                  "Cannot find %s in the context.\n  Choices are: %a.")
                 d_attrparam a
                 name
                 (docList (fun (s, _) -> text s)) ctx)
    end
    | ABinOp (bop, e1, e2) -> 
        let lv1', e1' = compile e1 in
        let lv2', e2' = compile e2 in
        (* now that we know the types of these expressions,
           fix any MinusA/PlusA that should be pointer arithmetic. *)
        let bop' = match bop, isPointer e1', isPointer e2' with
            MinusA, true, true -> MinusPP
          | MinusA, true, false -> MinusPI
          | PlusA, true, false -> PlusPI
          | _ -> bop
        in
        lv1' @ lv2', BinOp(bop', e1', e2', intType)
    | _ -> E.s (E.error "Cannot compile the dependency %a" d_attrparam a)
  in
  compile a

let rec boundsOfAttrs (ctx: context) (a:attributes) : exp*exp = 
  let checkrest rest =
    if hasAttribute "bounds" rest ||
       hasAttribute "fancybounds" rest then
      E.s (error "Type has duplicate bounds attributes")
  in
  match a with
  | Attr ("fancybounds", [AInt lo; AInt hi]) :: rest ->
      checkrest rest;
      getBoundsExp lo, getBoundsExp hi
  | Attr ("fancybounds", _) :: rest ->
      E.s (error "Illegal fancybounds annotations.")
  | Attr ("bounds", [lo; hi]) :: rest ->
      checkrest rest;
      (* Compile lo, hi into expressions *)
      let lodeps, lo' = compileAttribute ctx lo in
      let hideps, hi' = compileAttribute ctx hi in
      lo', hi'
  | Attr ("bounds", _) :: rest ->
      E.s (error "Illegal bounds annotations.")
  | Attr _ :: rest -> 
      boundsOfAttrs ctx rest
  | [] -> 
      E.s (error "Missing bounds annotations.")

let boundsOfType (ctx: context) (t: typ) : exp*exp =
  if !verbose then
    E.log "%a: boundsOfType %a\n" d_loc !currentLoc d_type t;
  match t with
  | TPtr (_, a) -> boundsOfAttrs ctx a
  | _ -> E.s (E.error "Expected pointer type.")

let makeFancyAttr (lo: exp) (hi: exp) : attribute =
  Attr ("fancybounds", [AInt (addBoundsExp lo); AInt (addBoundsExp hi)])

let makeFancyPtrType ?(nullterm:bool=false) (bt: typ) (lo: exp) (hi: exp) 
  : typ =
  let bounds_attr = [makeFancyAttr lo hi] in
  let attrs = if nullterm then 
    addAttribute (Attr("nullterm",[])) bounds_attr
  else
    bounds_attr
  in
  TPtr (bt, attrs)

(* Replace the names in type t with the corresponding expressions in ctx *)
let substType (ctx: context) (t: typ) : typ =
  if !verbose then
    E.log "%a: substType %a\n" d_loc !currentLoc d_type t;
  match t with
  | TPtr (bt, a) ->
      let lo, hi = boundsOfType ctx t in
      let a' =
        addAttribute (makeFancyAttr lo hi)
          (dropAttribute "bounds" (dropAttribute "fancybounds" a))
      in
      TPtr (bt, a')
  | _ ->
      t

let emptyContext : context = []

(* Add to the current context a binding for "__this" *)
let addThisBinding (ctx:context) (e:exp) : context =
  (thisKeyword, e)::ctx

(* Add to the current context a binding from name to e *)
let addBinding (ctx:context) (name:string) (e:exp) : context =
  (name, e)::ctx

(* The context of local and formal variables. *)
let localsContext (f:fundec) : context =
  List.fold_left
    (fun acc v -> (v.vname, Lval (var v)) :: acc)
    []
    (f.sformals @ f.slocals)

let structContext (lv: lval) (ci: compinfo) : context =
  List.fold_left
    (fun acc fld ->
       (fld.fname, Lval (addOffsetLval (Field (fld, NoOffset)) lv)) :: acc)
    []
    ci.cfields

(**************************************************************************)

let compareTypes (t1 : typ) (t2 : typ) : bool =
  (typeSig t1) = (typeSig t2)

(* Check that two types are the same. *)
let checkSameType (t1 : typ) (t2 : typ) : unit =
  if !verbose then
    E.log "%a: checkSameType on %a and %a\n" 
      d_loc !currentLoc
      d_type t1 d_type t2;
    match t1, t2 with
      TPtr (bt1, a1), TPtr (bt2, a2) ->
        if not (compareTypes bt1 bt2) then
          E.s (error "%a: base type mismatch: %a and %a\n" 
                 d_loc !currentLoc
                 d_type t1 d_type t2);
        (* Make sure the bounds are the same.
           We can use the empty context, because these should only contain 
           fancybounds *)
        let lo1, hi1 = boundsOfAttrs emptyContext a1 in
        let lo2, hi2 = boundsOfAttrs emptyContext a2 in
        (* Checking CIL expressions for equality statically is tricky.
           Do it dynamically: *)
        addCheck (CEq(lo1,lo2));
        addCheck (CEq(hi1,hi2))
    | _ -> 
        if not (compareTypes t1 t2) then
          E.s (error "%a: type mismatch: %a and %a\n" 
                 d_loc !currentLoc
                 d_type t1 d_type t2)
            

(* Add checks for a coercion of e from tfrom to tto. *)
(* matth: this has an ugly interface.  We should clean it up. *)
let coerceType 
  (ctx_from:context)         (* Context in which tfrom should be evaluated. *)
  ?(ctx_to:context=ctx_from) (* Context in which tto should be evaluated, 
                                if different from ctx_from. *)
  (e:exp)               (* Expression being coerced *)
  ~(tfrom : typ)        (* Type of e *)
  ~(tto : typ)          (* New type *)
  : unit =
  match tfrom, tto with
    TPtr(bt1, _), TPtr(bt2, _) when compareTypes bt1 bt2 ->
      if isNullterm tto && not (isNullterm tfrom) then
        E.s (error "%a: cast to NULLTERM from an ordinary pointer."
            d_loc !currentLoc);
      let lo_from, hi_from = boundsOfType (addThisBinding ctx_from e) tfrom in
      let lo_to, hi_to = boundsOfType (addThisBinding ctx_to e) tto in
      addCheck (CCoerce(lo_from, lo_to, e, hi_to, hi_from));
      ()
  | TInt _, TPtr _ when isZero e ->
      (* Coerce NULL to pointer.  Do we need to do any well-formedness checks
         here? *)
      ()
  | TInt _, TInt _ when (bitsSizeOf tfrom) = (bitsSizeOf tto) ->
      (* ignore signed/unsigned differences.  FIXME: is this safe? *)
      ()
  | _ -> 
    if not (compareTypes tfrom tto) then
      E.s (error "%a: type mismatch: coercion from %a to %a\n" 
             d_loc !currentLoc
             d_type tfrom d_type tto)
        

(* Calls checkExp e, then calls coerceType to make sure that
   e can be coerced to tto.*)
let rec coerceExp (ctx_from:context) ?(ctx_to:context=ctx_from)
                  (e:exp) (tto : typ) : unit =
  coerceType ctx_from ~ctx_to:ctx_to e ~tfrom:(checkExp ctx_from e) ~tto
        

and checkExp (ctx: context) (e : exp) : typ =
  match e with
  | UnOp (op, e', t) -> coerceType ctx e' ~tfrom:(checkExp ctx e') ~tto:t; t
  | BinOp ((PlusPI | IndexPI | MinusPI | MinusPP), e1, e2, t) ->
      let t1 = checkExp ctx e1 in
      let t2 = checkExp ctx e2 in
      (* FIXME: __this can appear in t, so we ignore it for now.
         At some point, we should check it! *)
      (* coerceType ctx e1 ~tfrom:t1 ~tto:t; *)
      coerceType ctx e2 ~tfrom:t2 ~tto:intType;
      let ctx' = addThisBinding ctx e1 in
      let lo, hi = boundsOfType ctx' t1 in
      addCheck (CNonNull e1);
      addCheck (CBounds (lo, e1, e2, hi));
      t1
  | BinOp (op, e1, e2, t) ->
      coerceExp ctx e1 t;
      coerceExp ctx e2 t;
      t
  | Lval lv -> checkLval ctx lv
  | CastE (t, e') -> coerceExp ctx e' t; t
  | SizeOfE e'
  | AlignOfE e' -> ignore(checkExp ctx e'); unrollType (typeOf e)
  | AddrOf lv ->
      if hasExternalDeps lv then
        E.s (E.error ("%a: cannot take address of lval " ^^
                      "with external dependencies\n") d_loc !currentLoc);
      let ctx' = addThisBinding ctx (Lval lv) in
      let bt = checkLval ctx' lv in
      let lo = AddrOf lv in
      let hi = BinOp (PlusPI, lo, one, typeOf lo) in
      makeFancyPtrType bt lo hi
  | StartOf lv ->
      let ctx' = addThisBinding ctx (Lval lv) in
      let bt, len =
        match unrollType (checkLval ctx' lv) with
        | TArray (bt, Some e, _) -> bt, e
        | TArray (_, None, _) -> E.s (E.error "array type has no length\n")
        | _ -> E.s (E.bug "expected array type\n")
      in
      let lo = StartOf lv in
      let hi = BinOp (PlusPI, lo, len, typeOf lo) in
      makeFancyPtrType bt lo hi
  | Const (CStr s) -> (* String literal *)
      let len = String.length s in
      let lo = e in
      let hi = BinOp (PlusPI, lo, integer len, typeOf lo) in
      makeFancyPtrType ~nullterm:true charType lo hi
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> unrollType (typeOf e)

and checkLval (ctx: context) (lv : lval) : typ =
  if !verbose then
    E.log "%a: checking %a\n" d_loc !currentLoc d_lval lv;
  begin
    match lv with
      Mem e, off -> begin
        let ctx' = addThisBinding ctx e in
        let ptrTy = checkExp ctx' e in
        let lo, hi = boundsOfType ctx' ptrTy in
        addCheck (CNonNull e);
        if not (isNullterm ptrTy) then
          addCheck (CNotEq(e,hi))
      end
    | Var vi, off -> ()
  end;
  let lv', off = removeOffsetLval lv in
  (* TODO: call checkExp on index offsets inside lv' *)
  match off with
  | NoOffset ->
      let ctx' = addThisBinding ctx (Lval lv) in
      substType ctx' (typeOfLval lv)
  | Field (fld, NoOffset) ->
      let ctx' = structContext lv' fld.fcomp in
      let ctx'' = addThisBinding ctx' (Lval lv) in
      substType ctx'' fld.ftype
  | Index (_, NoOffset) -> E.s (E.unimp "index offsets unsupported\n")
  | _ -> E.s (E.bug "unexpected result from removeOffset\n")



let checkCall (ctx: context) (lvo: lval option)
              (fn: exp) (args: exp list) : unit =
  match checkExp ctx fn with
  | TFun (returnType, argInfo, varargs, _) ->
      if varargs then
        E.s (E.unimp "varargs unimplemented\n");
      (match lvo with
       | Some lv -> 
           (* TODO: let the return type depend on formals *)
           if hasExternalDeps lv then
             E.s (E.error "%a: return lval has external dependencies\n"
                          d_loc !currentLoc);
           let lvType = checkLval ctx lv in
           (* replace __this in the return type with lv, and make sure the
              result equals the type of lv: *)
           let returnCtx = addThisBinding emptyContext (Lval lv) in
           let returnType' = substType returnCtx returnType in
           checkSameType returnType' lvType
       | None -> ()
      );
      (* CIL's casts don't make sense with dependent types, so remove them. *)
      let actuals = List.map stripCasts args in
      let formals = argsToList argInfo in
      begin
        try
          let ctxCall =
            List.fold_left2
              (fun ctxAcc (argName, argType, _) arg ->
                 if argName <> "" then
                   addBinding ctxAcc argName arg
                 else
                   ctxAcc)
              emptyContext
              formals
              actuals
          in
          List.iter2
            (fun (argName, argType, _) arg ->
               let ctx' = addThisBinding ctx arg in
               let ctxCall' = addThisBinding ctxCall arg in
               coerceExp ctx' ~ctx_to:ctxCall' arg argType)
            formals
            actuals
        with Invalid_argument _ ->
          E.s (E.bug "different number of formal and actual args\n")
      end
  | _ -> E.log "%a: calling non-function type\n" d_loc !currentLoc


let checkAlloc (ctx: context) (lv: lval) (bt:typ) (e: exp) : unit =
  if hasExternalDeps lv then
    E.s (E.error "%a: return lval has external dependencies\n"
                 d_loc !currentLoc);
  coerceExp ctx e intType;
  addCheck (CPositive e);
  let lo = Lval lv in
  let hi = BinOp(PlusPI, lo, e, TPtr(bt, [])) in (* FIXME: overflow *)
  let rt = makeFancyPtrType bt lo hi in
  let lvType = checkLval ctx lv in
  checkSameType rt lvType;
  ()


let checkSet (ctx: context) (lv: lval) (e: exp) : unit =
  if !verbose then
    E.log "%a: checking %a = %a\n" d_loc !currentLoc d_lval lv d_exp e;
  let lvType = checkLval ctx lv in
  let eType = checkExp ctx e in
  let off1, off2 = removeOffset (snd lv) in
  begin
    match off2 with
    | NoOffset ->
        begin
          match fst lv with
          | Var x when x.vglob ->
              (* TODO: handle globals *)
              E.s (E.unimp "global vars unimplemented\n")
          | Var x ->
              assert (not x.vglob);
              List.iter
                (fun y ->
                   let yExp = Lval (var y) in
                   let ySubst = if x.vname <> y.vname then yExp else e in
                   let ctxOld = addThisBinding ctx yExp in
                   let ctxNew =
                     addBinding (addThisBinding ctx ySubst) x.vname e
                   in
                   coerceExp ctxOld ySubst (substType ctxNew y.vtype))
                (!curFunc.slocals @ !curFunc.sformals)
          | Mem addr ->
              coerceExp ctx e lvType
        end
    | Field (x, NoOffset) ->
        List.iter
          (fun y ->
             let yExp =
               Lval (addOffsetLval (Field (y, NoOffset)) (fst lv, off1))
             in
             let ySubst = if x.fname <> y.fname then yExp else e in
             let ctxOld = addThisBinding ctx yExp in
             let ctxNew = addBinding (addThisBinding ctx ySubst) x.fname e in
             coerceExp ctxOld yExp (substType ctxNew y.ftype))
          x.fcomp.cfields
    | Field _ -> E.s (E.bug "unexpected field offset\n")
    | Index _ -> E.s (E.bug "index offsets not handled\n")
  end

let checkInstr (ctx: context) (instr : instr) : unit =
  currentLoc := get_instrLoc instr;
  match instr with
  (* Allocation *)
  | Call (Some lv, Lval (Var vi,NoOffset), [SizeOf t; e], _) 
      when vi.vname = "calloc" ->
      checkAlloc ctx lv t e
  | Call (_, Lval (Var vi,NoOffset), _, _) when vi.vname = "calloc" ->
      E.s (error "bad call to %s." vi.vname)
  (* Function call *)
  | Call (lvo, fn, args, _) ->
      checkCall ctx lvo fn args
  (* Assignment *)
  | Set (lv, e, _) ->
      (* CIL inserts a cast to vi.vtype.  This is wrong if vi.vtype has
         self-dependencies. So just ignore casts. *)
      checkSet ctx lv (stripCasts e)
  | Asm _ -> E.s (E.unimp "asm unsupported\n")

let rec checkStmt (ctx: context) (s : stmt) : unit =
  fixStmt s;
  curStmt := s.sid;
  currentLoc := get_stmtLoc s.skind;
  match s.skind with
  | Instr instrs ->
      List.iter (checkInstr ctx) instrs
  | Return (eo, _) ->
      begin
        match eo with
        | Some e ->
            let returnType =
              match !curFunc.svar.vtype with
              | TFun (returnType, _, _, _) -> returnType
              | _ -> E.s (E.bug "expected function type")
            in
            coerceExp ctx e returnType
        | None -> ()
      end
  | If (e, b1, b2, _) ->
      coerceExp ctx e intType;
      checkBlock ctx b1;
      checkBlock ctx b2;
  | Switch (e, b, _, _) ->
      coerceExp ctx e intType;
      checkBlock ctx b
  | Loop (b, _, _, _)
  | Block b -> checkBlock ctx b
  | Goto _
  | Break _
  | Continue _ -> ()
  | TryFinally _
  | TryExcept _ -> E.s (E.unimp "exceptions not supported\n")

and checkBlock (ctx: context) (b : block) : unit =
  List.iter (checkStmt ctx) b.bstmts

let checkFundec (fd : fundec) : unit =
  if !verbose then
    E.log "Doing function %s.\n" fd.svar.vname;
  curFunc := fd;
  let ctx = localsContext fd in
  checkBlock ctx fd.sbody;
  curFunc := dummyFunDec;
  curStmt := -1

let checkFile (f : file) : unit =
  List.iter
    (fun global ->
       match global with
       | GFun (fd, _) -> checkFundec fd
       | _ -> ())
    f.globals;
  (* Turn the check datastructure into explicit checks, so that they show up
     in the output. *)
  visitCilFileSameGlobals postPassVisitor f;
  f.globals <- (GText "#include <deputychecks.h>\n\n")::f.globals;
 (* Tell CIL to put comments around the bounds attributes. *)
  print_CIL_Input := false;
  ()

let feature : featureDescr = 
  { fd_name = "Deputy";
    fd_enabled = ref false;
    fd_description = "Typecheck and instrument the program using Deputy.";
    fd_extraopt = [
      "--deputyverbose", Arg.Set verbose, "Enable verbose output for Deputy";
(*    "--deputysuppress", Arg.Set suppress, "Suppress some Deputy warnings"; *)
    ];
    fd_doit = checkFile;
    fd_post_check = true;
  } 
