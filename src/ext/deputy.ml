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
module IH = Inthash
module RD = Reachingdefs
module RCT = Rmciltmps
module DF = Dataflow

let debug : bool ref = ref false
let verbose : bool ref = ref false
let trustAll : bool ref = ref false
let optLevel : int ref = ref 2
(** 0: no optimization
    1: flow-insensitive optimization
    2: all optimization *)
let inferFile : string ref = ref ""

(* Be careful when converting int64 to int.  Int64.to_int
   treats 2^31 the same as 0 *)
let to_int (i: int64) : int = 
  let i': int = Int64.to_int i in (* i.e. i' = i mod 2^31 *)
  if i = Int64.of_int i' then i'
  else E.s (error "Int constant too large: %Ld\n" i)

let isPointer e: bool =
  isPointerType (typeOf e)

(**************************************************************************)

let d_thisloc () : doc = d_loc () !currentLoc

let bug (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =  
    E.hadErrors := true;
    ignore (eprintf "%t: Bug: %a@!" d_thisloc insert d);
    flush !E.logChannel
  in
  Pretty.gprintf f fmt

let error (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%t: Error: %a@!" d_thisloc insert d);
    flush !E.logChannel
  in
  Pretty.gprintf f fmt

let unimp (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%t: Unimplemented: %a@!" d_thisloc insert d);
    flush !E.logChannel
  in
  Pretty.gprintf f fmt

let warn (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =
    ignore (eprintf "%t: Warning: %a@!" d_thisloc insert d);
    flush !E.logChannel
  in
  Pretty.gprintf f fmt

let log (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =
    ignore (eprintf "%t: %a@!" d_thisloc insert d);
    flush !E.logChannel
  in
  Pretty.gprintf f fmt

let errorwarn (fmt : ('a,unit,doc,unit) format4) : 'a = 
  let f d =
    if !trustAll then
      warn "%a" insert d
    else
      E.s (error "%a" insert d)
  in
  Pretty.gprintf f fmt

(**************************************************************************)

let curFunc : fundec ref = ref dummyFunDec
let curStmt : int ref = ref (-1)

let staticGlobalVars : varinfo list ref = ref []
let nonStaticGlobalVars : varinfo list ref = ref []

let exemptLocalVars : varinfo list ref = ref []

(* Assign to each statement a unique ID. *)
let nextStmtId : int ref = ref 0
let assignID (s:stmt) : unit =
 (* Make sure that no one else has assigned ID numbers *)
  if !optLevel < 2 && s.sid <> -1 then
    E.s (bug "Stmt already has an sid: %a\n" d_stmt s);
  s.sid <- !nextStmtId;
  incr nextStmtId;
  ()

(* Convert instruction lists into individual statements, and give each
  stmt a unique id. *)
let rec fixStmt (s:stmt) : unit =
  assignID s;
  match s.skind with 
    Instr [] -> ()
  | Instr [i] -> ()
  | Instr il -> (* Two or more instructions *)
      let sl = List.map mkStmtOneInstr il in
      List.iter fixStmt sl;
      s.skind <- Block (mkBlock sl);
      ()
  | If(_,b1,b2,_) ->
      fixBlock b1;
      fixBlock b2
  | Switch(_,b,_,_) ->
      fixBlock b
  | Loop(b,_,_,_) ->
      fixBlock b
  | Block b -> fixBlock b
  | TryFinally(b1,b2,_) ->
      fixBlock b1;
      fixBlock b2
  | TryExcept(b1,_,b2,_) ->
      fixBlock b1;
      fixBlock b2
  | _ -> ()

and fixBlock (b : block) : unit =
  List.iter fixStmt b.bstmts

(* Splits a list into two lists consisting of the first n elements
 * and the remainder. *)
let rec split (l: 'a list) (n: int) : 'a list * 'a list =
  match l with
  | elt :: rest when n > 0 ->
      let x, y = split rest (n - 1) in
      elt :: x, y
  | _ -> [], l

(* Like iter, but passes an int indicating the current index in the list. *)
let iterindex (fn: 'a -> int -> unit) (a: 'a list) : unit =
  let rec helper a n =
    match a with
    | [] -> ()
    | a1 :: arest -> fn a1 n; helper arest (n + 1)
  in
  helper a 1

(* Like iter2, but passes an int indicating the current index in the lists. *)
let iter2index (fn: 'a -> 'b -> int -> unit) (a: 'a list) (b: 'b list) : unit =
  let rec helper a b n =
    match a, b with
    | [], [] -> ()
    | a1 :: arest, b1 :: brest -> fn a1 b1 n; helper arest brest (n + 1)
    | _ -> raise (Invalid_argument "iter2index")
  in
  helper a b 1

let rec typeContainsPointers (t: typ) : bool =
  match t with
  | TPtr _
  | TFun _
  | TBuiltin_va_list _ -> true
  | TVoid _
  | TInt _
  | TFloat _
  | TEnum _ -> false
  | TArray (bt, _, _) -> typeContainsPointers bt
  | TNamed (ti, _) -> typeContainsPointers ti.ttype
  | TComp (ci, _) ->
     List.exists typeContainsPointers
      (List.map (fun fld -> fld.ftype) ci.cfields)

let rec compareExp (e1: exp) (e2: exp) : bool =
(*   log "CompareExp %a and %a.\n" d_plainexp e1 d_plainexp e2; *)
  e1 == e2 ||
  match e1, e2 with
  | Lval lv1, Lval lv2
  | StartOf lv1, StartOf lv2
  | AddrOf lv1, AddrOf lv2 -> compareLval lv1 lv2
  | BinOp(bop1, l1, r1, _), BinOp(bop2, l2, r2, _) -> 
      bop1 = bop2 && compareExp l1 l2 && compareExp r1 r2
  | _ -> begin
      match isInteger (constFold true e1), isInteger (constFold true e2) with
        Some i1, Some i2 -> i1 = i2
      | _ -> false
    end

and compareLval (lv1: lval) (lv2: lval) : bool =
  let rec compareOffset (off1: offset) (off2: offset) : bool =
    match off1, off2 with
    | Field (fld1, off1'), Field (fld2, off2') ->
        fld1 == fld2 && compareOffset off1' off2'
    | Index (e1, off1'), Index (e2, off2') ->
        compareExp e1 e2 && compareOffset off1' off2'
    | NoOffset, NoOffset -> true
    | _ -> false
  in
  lv1 == lv2 ||
  match lv1, lv2 with
  | (Var vi1, off1), (Var vi2, off2) ->
      vi1 == vi2 && compareOffset off1 off2
  | (Mem e1, off1), (Mem e2, off2) ->
      compareExp e1 e2 && compareOffset off1 off2
  | _ -> false


(* Remove casts that do not effect the value of the expression, such
 * as casts between different pointer types.  Of course, these casts
 * change the type, so don't use this within e.g. an arithmetic
 * expression.
 * 
 * We also prune casts between equivalent integer types, such as a
 * difference in sign or int vs long.  But we keep other arithmetic casts,
 * since they actually change the value of the expression. *)
let rec stripNopCasts (e:exp): exp =
  match e with
    CastE(t, e') -> begin
      match unrollType t, unrollType (typeOf e') with
        TPtr _, TPtr _ -> (* okay to strip *)
          stripNopCasts e'
      | (TInt _ as t1), (TInt _ as t2) 
          when bitsSizeOf t1 = bitsSizeOf t2 -> (* Okay to strip.*)
          stripNopCasts e'
      |  _ -> e
    end
  | _ -> e
      
let compareExpStripCasts (e1: exp) (e2: exp) : bool =
  compareExp (stripNopCasts e1) (stripNopCasts e2)


let stripOneCast (e: exp) : exp =
  match e with
  | CastE (_, e') -> e'
  | _ -> e

let isNullterm (t: typ) : bool =
  match unrollType t with
  | TPtr (_, a) -> hasAttribute "nullterm" a
  | _ -> E.s (error "Expected pointer type")

let isPoly (t: typ) : bool =
  match unrollType t with
  | TPtr (TVoid _, a) -> hasAttribute "poly" a
  | _ -> false

let hasPolyId (t: typ) : bool =
  match filterAttributes "poly" (typeAttrs t) with
  | [Attr ("poly", [AInt _])] -> true
  | _ -> false

let getPolyId (t: typ) : int =
  match filterAttributes "poly" (typeAttrs t) with
  | [Attr ("poly", [AInt n])] -> n
  | [] -> E.s (bug "Expected poly attribute")
  | _ -> E.s (bug "Unexpected poly attribute args")

let isTrustedAttr (attr: attributes) : bool =
  hasAttribute "trusted" attr

let isTrustedType (t: typ) : bool =
  isTrustedAttr (typeAttrs t)

let isTrustedComp (ci: compinfo) : bool =
  isTrustedAttr ci.cattr

(* Keyword in bounds attributes representing the current value *)
let thisKeyword = "__this"

(* Note that we use PlusA here instead of PlusPI in order to match actual
 * annotations as parsed by CIL. *)
let countAttr (a: attrparam) : attribute =
  Attr ("bounds", [ACons (thisKeyword, []);
                   ABinOp (PlusA, ACons (thisKeyword, []), a)])

let safeAttr : attribute = countAttr (AInt 1)

let sentinelAttr : attribute =
  Attr ("bounds", [ACons (thisKeyword, []); ACons (thisKeyword, [])])

(**************************************************************************)

type check =
    CNonNull of exp      (** e != 0 *)
  | CEq of exp * exp     (** e1 == e2 *)
  | CPositive of exp     (** e > 0 *)
  | CMult of exp * exp   (** e1 * k == e2 for some int k *)
  | COverflow of exp * exp
                         (** e1 + e2 does not overflow (e2 is signed) *)
  | CUnsignedLess of exp * exp * string
                         (** e1 < e2, unsigned.
                           * Also remember why this check was added. *)
  | CUnsignedLE of exp * exp * string
                         (** e1 <= e2, unsigned.
                           * Also remember why this check was added. *)
  | CUnsignedLENT of exp * exp * string
                         (** e1 <= (e2 + sizeof(e2)), unsigned.
                           * Also remember why this check was added. *)
  | CNullOrLE of exp * exp * exp * string
                         (** e1 == 0 || e2 <= e3.
                           * Also remember why this check was added. *)
  | CNullOrLENT of exp * exp * exp * string
                         (** e1 == 0 || e2 <= (e3 + sizeof(e3)).
                           * Also remember why this check was added. *)
  | CNTWrite of exp * exp * exp
                         (** (e1 == e2) ==> (e3 = 0)   *)
  | CNullUnion of lval   (** e = \vec{0} *)
  (* These two are redundant with CNonNull and CEq, but having separate
     checks for unions gives better error messages: *)
  | CSelected of exp     (** e != 0 *)
  | CNotSelected of exp  (** e == 0 *)
(* Other checks will be needed, such as nullterm checks and checks for when
   part of one of the above checks can be proved statically. *)


(* A mapping from stmt ids to checks for that instruction. The list of checks 
   is stored in reverse order.  *)
let allowChecks : bool ref = ref false
let allChecks : check list GA.t = GA.make 200 (GA.Elem [])

(* Add a check for the current statement.
 * When more than one check is added for a statement, they will be executed in
 * the order that addCheck was called.
 * For example, when accessing "**e", call addCheck (CNonNull e)
 * first, then addCheck (CNonNull *e).  Then we'll be sure to check e 
 * before *e, which ensures that the *e check won't segfault. 
 *)
let addCheck (c:check) : unit =
  if !allowChecks then begin
    let id = !curStmt in
    if id < 0 then 
      E.s (bug "addCheck when sid = %d.\n" id);
    let otherChecks = GA.getg allChecks id in
    if !optLevel = 0 || not (List.mem c otherChecks) then
      GA.set allChecks id (c::otherChecks)
  end

(* These aren't real variables.  In the output, they'll show up as
   __FILE__ and __LINE__, and gcc will see them as macros.  We use them
   for calling runtime check functions. *)
let fileToken : exp=
  let vi = makeGlobalVar "__FILE__" charPtrType in
  Lval (var vi)
let lineToken : exp=
  let vi = makeGlobalVar "__LINE__" intType in
  Lval (var vi)

let mkFun (name: string) (rt:typ) (args: typ list) : exp = 
  let fdec = emptyFunction name in
  let args = List.map (fun t -> ("", t, [])) args in
  fdec.svar.vtype <- TFun(rt, Some args, false, []);
  Lval (var fdec.svar)
let mkCheckFun (name: string) (n: int) : exp = 
 (* A check function takes n void* parameters, a file name, and a line number*)
  let args = Util.list_init n (fun _ -> voidPtrType) in
  let args' = args @ [charPtrType; intType] in
  mkFun name voidType args'
let cnonnull = mkCheckFun "CNonNull" 1
let ceq = mkCheckFun "CEq" 2
let cpositive = mkCheckFun "CPositive" 1
let cmult = mkCheckFun "CMult" 2
let coverflow = mkCheckFun "COverflow" 3
let cunsignedless = mkCheckFun "CUnsignedLess" 3
let cunsignedle = mkCheckFun "CUnsignedLE" 3
let cunsignedlent = mkCheckFun "CUnsignedLENT" 3
let cnullorle = mkCheckFun "CNullOrLE" 4
let cnullorlent = mkCheckFun "CNullOrLENT" 4
let cntwrite = mkCheckFun "CNTWrite" 5
let cnullunion = mkCheckFun "CNullUnion" 2
let cselected = mkCheckFun "CSelected" 1
let cnotselected = mkCheckFun "CNotSelected" 1
let memset = mkFun "memset" voidType [voidPtrType; intType; !upointType]
let strlen = mkFun "deputy_strlen" intType
                   [typeAddAttributes [sentinelAttr] charPtrType]

let checkToInstr (c:check) =
  let call f args = Call(None, f,
                         (* Append the file and line to the end of the args *)
                         args @ [fileToken; lineToken],
                         !currentLoc) 
  in
  match c with
    CNonNull (e) -> call cnonnull [e]
  | CEq (e1,e2) -> call ceq [e1;e2]
  | CPositive (e) -> call cpositive [e]
  | CMult (e1,e2) -> call cmult [e1;e2]
  | COverflow (e1,e2) -> call coverflow [e1;e2]
  | CUnsignedLess (e1,e2,why) -> call cunsignedless [e1;e2; mkString why]
  | CUnsignedLE (e1,e2,why) -> call cunsignedle [e1;e2; mkString why]
  | CUnsignedLENT (e1,e2,why) -> call cunsignedlent [e1;e2; mkString why]
  | CNullOrLE (e1,e2,e3,why) -> call cnullorle [e1;e2;e3; mkString why]
  | CNullOrLENT (e1,e2,e3,why) -> call cnullorlent [e1;e2;e3; mkString why]
  | CNTWrite (p,hi,what) -> call cntwrite [p;hi;what]
  | CNullUnion (lv) -> 
      let sz = sizeOf (typeOfLval lv) in
      call cnullunion [mkAddrOf lv; sz]
  | CSelected (e) -> call cselected [e]
  | CNotSelected (e) -> call cnotselected [e]

let addBoundsCheck (lo: exp) (e: exp) (hi: exp) (nt: bool) : unit =
  let why = "Bounds" in
  addCheck (CUnsignedLE(lo, e, why));
  if nt then
    addCheck (CUnsignedLENT(e, hi, why))
  else
    addCheck (CUnsignedLE(e, hi, why));
  ()

(* Checks that ptr != 0,
   lo <= (ptr+off) <= hi,
   and ptr+off does not overflow *)
let addArithChecks (lo: exp) (ptr: exp) (off : exp) (hi : exp) : unit =
  addCheck (CNonNull ptr);
  addCheck (COverflow (ptr, off));
  let e = BinOp (PlusPI, ptr, off, typeOf ptr) in
  let nt = isNullterm (typeOf ptr) in
  addBoundsCheck lo e hi nt

let addCoercionCheck ?(fromNullterm=false) (lo_from: exp) (lo_to: exp) (e: exp)
                     (hi_to: exp) (hi_from: exp) (t: typ) : unit =
  (* If the lower bound has changed, do an lbound check. 
   * (we already know that lo_from <= e, so if lo_from=lo_to,
   *  we don't have to check that lo_to <= e)
  *)
  let why = "Coerce" in
  if !optLevel = 0 || not (compareExpStripCasts lo_from lo_to) then begin
    addCheck (CNullOrLE(e, lo_from, lo_to, why));
    addCheck (CNullOrLE(e, lo_to, e, why));
  end;
  if !optLevel = 0 || not (compareExpStripCasts hi_from hi_to) then begin
    addCheck (CNullOrLE(e, e, hi_to, why));
    if fromNullterm then
      addCheck (CNullOrLENT(e, hi_to, hi_from, why))
    else
      addCheck (CNullOrLE(e, hi_to, hi_from, why));
  end;
  ()


(**************************************************************************)

(* remember complicated bounds expressions *)
let boundsTable : exp IH.t = IH.create 13
let boundsTableCtr : int ref = ref 0

let addBoundsExp (e: exp) : int =
  incr boundsTableCtr;
  if !verbose then
    E.log "%a:   fancybounds(%d) = %a.\n" d_loc !currentLoc
      !boundsTableCtr d_exp e;
  IH.add boundsTable !boundsTableCtr e;
  !boundsTableCtr

let getBoundsExp (n: int) : exp =
  try
    IH.find boundsTable n
  with Not_found ->
    E.s (bug "Couldn't look up expression in bounds table")

let clearBoundsTable () : unit =
  IH.clear boundsTable;
  boundsTableCtr := 0

type paramkind =
| PKNone
| PKThis
| PKOffset of attrparam

class deputyPrinterClass : cilPrinter = object (self)
  inherit defaultCilPrinterClass as super

  method pAttr (Attr (an, args) : attribute) : doc * bool =
    let rec checkParam (ap: attrparam) : paramkind =
      match ap with
      | ACons (name, []) when name = thisKeyword -> PKThis
      | ABinOp (PlusA, a1, a2) when checkParam a1 = PKThis ->
          if a2 = AInt 0 then PKThis else PKOffset a2
      | _ -> PKNone
    in
    match an, args with 
    | "fancybounds", [AInt i1; AInt i2] ->
        let d1 = self#pExp () (getBoundsExp i1) in
        let d2 = self#pExp () (getBoundsExp i2) in
        text "BND(" ++ d1 ++ text ", " ++ d2 ++ text ")", false
    | "bounds", [a1; a2] ->
        begin
          match checkParam a1, checkParam a2 with
          | PKThis, PKThis ->
              text "SNT", false
          | PKThis, PKOffset (AInt 1) ->
              text "SAFE", false
          | PKThis, PKOffset a ->
              let d = self#pAttrParam () a in
              text "COUNT(" ++ d ++ text ")", false
          | _ ->
              let d1 = self#pAttrParam () a1 in
              let d2 = self#pAttrParam () a2 in
              text "BND(" ++ d1 ++ text ", " ++ d2 ++ text ")", false
        end
    | "fancywhen", [AInt i] ->
        let d = self#pExp () (getBoundsExp i) in
        text "WHEN(" ++ d ++ text ")", false
    | "when", [a] ->
        let d = self#pAttrParam () a in
        text "WHEN(" ++ d ++ text ")", false
    | "nullterm", [] ->
        text "NULLTERM", false
    | "trusted", [] ->
        text "TRUSTED", false
    | "poly", [a] ->
        let d = self#pAttrParam () a in
        text "POLY(" ++ d ++ text ")", false
    | "poly", [] ->
        text "POLY", false
    | _ ->
        super#pAttr (Attr (an, args))
end

let deputyPrinter = new deputyPrinterClass

let dx_type () (t: typ) : doc =
  printType deputyPrinter () t

let dx_exp () (e: exp) : doc =
  printExp deputyPrinter () e

(* remember complicated WHEN expressions. For each union in each context,
   we have a whenMap, which maps fields to the expanded when condition for that
   field in the context.  *)
type whenMap = (fieldinfo * exp) list
let whenTable : whenMap IH.t = IH.create 13
let whenTableCtr : int ref = ref 0
let d_whenMap () (wm:whenMap) :  doc =
  Pretty.align ++
  docList ~sep:line 
    (fun (f,e) -> text f.fname ++ text ": " ++ dx_exp () e)
    () wm
  ++ Pretty.unalign

let addWhenMap (wm:whenMap) : int =
  incr whenTableCtr;
  if !verbose then
    E.log "%a:   fancywhen(%d) = [%a].\n" d_loc !currentLoc
      !whenTableCtr d_whenMap wm;
  IH.add whenTable !whenTableCtr wm;
  !whenTableCtr

let getWhenMap (n: int) : whenMap =
  try
    IH.find whenTable n
  with Not_found ->
    E.s (E.bug "couldn't look up %d in when table\n" n)

let rec getDeps (a: attrparam) : string list =
  match a with 
  | AInt k -> []
  | ASizeOf t -> []
  | ASizeOfE e -> []
  | ACons(name, []) -> [name]
  | ABinOp (_, e1, e2) -> (getDeps e1) @ (getDeps e2)
  | _ -> E.s (error "Cannot get dependencies for %a" d_attrparam a)

let rec depsOfAttrs (a: attributes) : string list = 
  let checkrest rest =
    if hasAttribute "bounds" rest ||
       hasAttribute "fancybounds" rest then
      E.s (error "Type has duplicate bounds attributes: %a" d_attrlist a)
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
      E.s (bug "Missing bounds information")

let depsOfType (t: typ) : string list =
  match t with
  | TPtr (_, a) -> depsOfAttrs a
  | _ -> []

let rec getWhen (a: attributes) : attrparam =
  let checkrest rest =
    if hasAttribute "when" rest then
      E.s (error "Field has more than one WHEN attribute")
  in
  match a with
  | Attr ("when", [e]) :: rest ->
      checkrest rest;
      e
  | Attr ("when", _) :: rest ->
      E.s (error "Illegal when annotations.")
  | Attr _ :: rest -> 
      getWhen rest
  | [] -> 
      raise Not_found

let depsOfWhenAttrs (a: attributes) : string list = 
  let w = getWhen a in
  getDeps w

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
        | Var vi ->
            let env =
              if not vi.vglob then
                !curFunc.slocals @ !curFunc.sformals
              else if vi.vglob && vi.vstorage = Static then
                !staticGlobalVars
              else
                [vi]
            in
            let vars = List.map (fun vi -> vi.vname, vi.vtype) env in
            hasDeps vi.vname vars
        | Mem e ->
            false
      end
  | Field (fld, NoOffset) ->
      let vars =
        List.map (fun fld -> fld.fname, fld.ftype) fld.fcomp.cfields
      in
      hasDeps fld.fname vars
  | Index (_, NoOffset) ->
      (* No one depends on array elements.  
         FIXME: what about arrays inside null-terminated arrays? *)
      false
  | _ -> E.s (bug "Unexpected result from removeOffset")

(* A context maps variable/field names to the corresponding CIL expr. *)
type context = (string * exp) list

let localsContext (f:fundec) : context =
  List.fold_left
    (fun acc v -> (v.vname, Lval (var v)) :: acc)
    []
    (f.sformals @ f.slocals)

let globalsContext () : context =
  List.fold_left
    (fun acc v -> (v.vname, Lval (var v)) :: acc)
    []
    !staticGlobalVars

let structContext (lv: lval) (ci: compinfo) : context =
  List.fold_left
    (fun acc fld ->
       (fld.fname, Lval (addOffsetLval (Field (fld, NoOffset)) lv)) :: acc)
    []
    ci.cfields

let allContext () : context =
  List.fold_left
    (fun acc v -> (v.vname, Lval (var v)) :: acc)
    []
    (!staticGlobalVars @ !nonStaticGlobalVars @
     !curFunc.sformals @ !curFunc.slocals)

(** The dependent types are expressed using attributes. We compile an 
 * attribute given a mapping from names to lvals.  Returns the names of
 * meta values that this annotation depends on, and the expression.
 *  
 * This is a helper for both fields and formals. *)
let rec compileAttribute 
  (ctx: context) (* Should include a mapping for thisKeyword *)
  (a: attrparam) 
  : string list * exp = 
  let rec compile (a: attrparam) = 
    match a with 
      AInt k -> [], integer k
    | ASizeOf t -> [], SizeOf t
    | ASizeOfE e ->
        let _, e' = compileAttribute (allContext ()) e in
        [], SizeOfE e'
    | ACons(name, []) -> begin
        try 
          let e = List.assoc name ctx in 
          [name], e
        with Not_found -> 
          E.s (error 
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
        lv1' @ lv2', BinOp(bop', e1', e2', typeOf e1')
    | _ -> E.s (error "Cannot compile the dependency %a" d_attrparam a)
  in
  compile a

type bounds =
| BSimple of attrparam * attrparam
| BFancy of exp * exp

let rec getBounds (a: attributes) : bounds =
  let checkrest rest =
    if hasAttribute "bounds" rest ||
       hasAttribute "fancybounds" rest then
      E.s (error "Type has duplicate bounds attributes: %a" d_attrlist a)
  in
  match a with
  | Attr ("bounds", [lo; hi]) :: rest ->
      checkrest rest;
      BSimple (lo, hi)
  | Attr ("fancybounds", [AInt lo; AInt hi]) :: rest ->
      checkrest rest;
      BFancy (getBoundsExp lo, getBoundsExp hi)
  | Attr _ :: rest -> 
      getBounds rest
  | [] -> 
      E.s (bug "Missing bounds information")

let boundsOfAttrs (ctx: context) (a: attributes) : exp * exp = 
  match getBounds a with
  | BSimple (lo, hi) ->
      (* Compile lo, hi into expressions *)
      let lodeps, lo' = compileAttribute ctx lo in
      let hideps, hi' = compileAttribute ctx hi in
      lo', hi'
  | BFancy _ ->
      E.s (error "Found fancybounds instead of bounds annotations")

let fancyBoundsOfAttrs (a: attributes) : exp * exp = 
  match getBounds a with
  | BSimple (lo, hi) ->
      E.s (error "Found bounds instead of fancybounds annotations")
  | BFancy (lo, hi) ->
      lo, hi

let fancyBoundsOfType (t: typ) : exp * exp =
  if !verbose then
    E.log "%a: fancyBoundsOfType %a\n" d_loc !currentLoc dx_type t;
  match unrollType t with
  | TPtr (_, a) -> fancyBoundsOfAttrs a
  | _ -> E.s (error "Expected pointer type")

let makeFancyBoundsAttr (lo: exp) (hi: exp) : attribute =
  Attr ("fancybounds", [AInt (addBoundsExp lo); AInt (addBoundsExp hi)])

let makeFancyPtrType ?(nullterm:bool=false) (bt: typ) (lo: exp) (hi: exp) 
  : typ =
  let bounds_attr = [makeFancyBoundsAttr lo hi] in
  let attrs = if nullterm then 
    addAttribute (Attr("nullterm",[])) bounds_attr
  else
    bounds_attr
  in
  TPtr (bt, attrs)

let whenOfAttrs (ctx: context) (a: attributes) : exp =
  let w = getWhen a in
  let deps, e = compileAttribute ctx w in
  e

let makeFancyWhenAttr (wm: whenMap) : attribute =
  Attr ("fancywhen", [AInt (addWhenMap wm)])

let fancyWhenOfType (t: typ) : whenMap =
  match unrollType t with
  | TComp (_, a) -> begin
      match filterAttributes "fancywhen" a with
        [Attr("fancywhen", [AInt i])] -> getWhenMap i
      | _ -> E.s (bug "missing (or malformed) fancywhen: %a" d_attrlist a)
    end
  | _ -> E.s (E.bug "Expected union type.")

(* Replace the names in type t with the corresponding expressions in ctx *)
let substType (ctx: context) (t: typ) : typ =
  if !verbose then
    E.log "%a: substType %a\n" d_loc !currentLoc dx_type t;
  match unrollType t with
  | TPtr (bt, a) ->
      let lo, hi = boundsOfAttrs ctx a in
      let a' = addAttribute (makeFancyBoundsAttr lo hi) 
                 (dropAttribute "bounds" a) in
      TPtr (bt, a')
  | TComp (ci, a) when not ci.cstruct && not (isTrustedComp ci) ->
      (* a union. Create a fancywhen attr for the when clauses of each field.*)
      let doField (acc:whenMap) (fld:fieldinfo) : whenMap =
        try 
          let e : exp = whenOfAttrs ctx fld.fattr in (* may raise Not_found *)
          (fld, e) :: acc
        with Not_found ->
          if typeContainsPointers fld.ftype then begin
            E.s (bug "Missing WHEN annotation on field %s.\n" fld.fname)
          end else
            (* Allow missing WHEN clauses for scalars. *)
            acc
      in
      let wm = List.fold_left doField [] ci.cfields in
      let a' = addAttribute (makeFancyWhenAttr wm) a in
      TComp (ci, a')
  | _ ->
      t

let emptyContext : context = []

(* Add to the current context a binding for "__this" *)
let addThisBinding (ctx:context) (e:exp) : context =
  (thisKeyword, e)::ctx

(* Add to the current context a binding from name to e *)
let addBinding (ctx:context) (name:string) (e:exp) : context =
  (name, e)::ctx

(* Check whether a binding exists. *)
let hasBinding (ctx:context) (name:string) : bool =
  List.exists (fun (n, _) -> n = name) ctx
let hasBindings (ctx:context) (names : string list) : bool =
  List.for_all (hasBinding ctx) names

(**************************************************************************)

let compareTypes (t1 : typ) (t2 : typ) : bool =
  let typeSigNC (t : typ) : typsig =
    let attrFilter (attr : attribute) : bool =
      match attr with
      | Attr ("poly", _) (* TODO: hack hack! *)
      | Attr ("const", [])
      | Attr ("volatile", [])
      | Attr ("always_inline", []) -> false
      | _ -> true
    in
    typeSigWithAttrs (List.filter attrFilter) t
  in
  (typeSigNC t1) = (typeSigNC t2)

let isAllocator (t: typ) : bool =
  let attrs = typeAttrs t in
  hasAttribute "dmalloc" attrs || hasAttribute "dcalloc" attrs

let isMemset (t: typ) : bool =
  hasAttribute "dmemset" (typeAttrs t)

let isMemcpy (t: typ) : bool =
  hasAttribute "dmemcpy" (typeAttrs t)

let isMemcmp (t: typ) : bool =
  hasAttribute "dmemcmp" (typeAttrs t)

let getMallocArg (attrs: attributes) : int =
  if hasAttribute "dcalloc" attrs then
    E.s (error "Function has too many allocator annotations");
  match filterAttributes "dmalloc" attrs with
  | [Attr (_, [AInt i])] -> i - 1
  | [Attr (_, _)] -> E.s (error "Invalid malloc annotation")
  | _ :: _ -> E.s (error "Function has too many allocator annotations")
  | [] -> E.s (bug "No dmalloc attribute found")

let getCallocArgs (attrs: attributes) : int * int =
  if hasAttribute "dmalloc" attrs then
    E.s (error "Function has too many allocator annotations");
  match filterAttributes "dcalloc" attrs with
  | [Attr (_, [AInt i1; AInt i2])] -> i1 - 1, i2 - 1
  | [Attr (_, _)] -> E.s (error "Invalid calloc annotation")
  | _ :: _ -> E.s (error "Function has too many allocator annotations")
  | [] -> E.s (bug "No dcalloc attribute found")

let rec expToAttr (e: exp) : attrparam option =
  match stripNopCasts e with
  | Lval (Var vi, NoOffset) -> Some (ACons (vi.vname, []))
  | Const _ ->
      begin
        match isInteger e with
        | Some i -> Some (AInt (to_int i))
        | None -> None
      end
  | BinOp ((MinusA | PlusA) as op, e1, e2, _) ->
      begin
        match expToAttr e1, expToAttr e2 with
        | Some a1, Some a2 -> Some (ABinOp (op, a1, a2))
        | _ -> None
      end
  | _ -> None

let getAllocationType (retType: typ) (fnType: typ) (args: exp list) : typ =
  let fnAttrs = typeAttrs fnType in
  let numElts, baseType =
    if hasAttribute "dcalloc" fnAttrs then
      let i1, i2 = getCallocArgs fnAttrs in
      try
        match List.nth args i1, stripNopCasts (List.nth args i2) with
        | e, SizeOf t' -> e, t'
        | e, SizeOfE et -> e, typeOf et
        | _ -> E.s (error "Unrecognized calloc arguments")
      with Failure "nth" ->
        E.s (error "Invalid indices in calloc annotation")
    else if hasAttribute "dmalloc" fnAttrs then
      let i = getMallocArg fnAttrs in
      try
        match stripNopCasts (List.nth args i) with
        | BinOp (Mult, e', SizeOf t, _)
        | BinOp (Mult, SizeOf t, e', _) -> e', t
        | BinOp (Mult, e', SizeOfE et, _)
        | BinOp (Mult, SizeOfE et, e', _) -> e', typeOf et
        | SizeOf t -> integer 1, t
        | SizeOfE et -> integer 1, typeOf et
        | e -> e, charType
      with Failure "nth" ->
        E.s (error "Invalid index in malloc annotation")
    else
      E.s (error "Unrecognized allocation function")
  in
  let retBaseType =
    match unrollType retType with
    | TPtr (bt, _) -> bt
    | _ -> E.s (error "Return type of allocation is not a pointer")
  in
  if !verbose then
    log "Allocating %a elements with type %a\n" dx_exp numElts dx_type baseType;
  if not (compareTypes baseType retBaseType) then
    errorwarn "Type mismatch: alloc type %a and return type %a differ"
              dx_type baseType dx_type retBaseType;
  match expToAttr numElts with
  | Some a -> typeAddAttributes [countAttr a]
                (typeRemoveAttributes ["bounds"] retType)
  | None -> E.s (error "Cannot convert alloc expression to type: %a"
                 dx_exp numElts)

(* Check that two types are the same. *)
let checkSameType (t1 : typ) (t2 : typ) : unit =
  if !verbose then
    E.log "%a: checkSameType on %a and %a\n" 
      d_loc !currentLoc
      dx_type t1 dx_type t2;
    match unrollType t1, unrollType t2 with
    | t1, t2 when isTrustedType t1 || isTrustedType t2 ->
        ()
    | TPtr (bt1, a1), TPtr (bt2, a2) ->
        if not (compareTypes bt1 bt2) then
          errorwarn "Base type mismatch: %a and %a" dx_type t1 dx_type t2;
        (* Make sure the bounds are the same.
           We can use the empty context, because these should only contain 
           fancybounds *)
        let lo1, hi1 = fancyBoundsOfAttrs a1 in
        let lo2, hi2 = fancyBoundsOfAttrs a2 in
        (* Checking CIL expressions for equality statically is tricky.
           Do it dynamically: *)
        (* FIXME: this may be broken when used for calls, because the return
           value isn't known yet.  
           Or maybe this is okay if all we need to check is the relative
           locations of the bounds?    -- Matt *)
        addCheck (CEq(lo1,lo2));
        addCheck (CEq(hi1,hi2))
    | TInt _, TInt _ when (bitsSizeOf t1) = (bitsSizeOf t2) ->
        (* ignore signed/unsigned differences.  FIXME: is this safe? *)
        ()
    | _ -> 
        if not (compareTypes t1 t2) then
          errorwarn "Type mismatch: %a and %a" dx_type t1 dx_type t2

let checkUnionWhen (ctx:context) (fld:fieldinfo) : bool =
  isTrustedComp fld.fcomp ||
  try 
    let deps = depsOfWhenAttrs fld.fattr in (* may raise Not_found *)
    hasBindings ctx deps
  with Not_found ->
    if typeContainsPointers fld.ftype then begin
      E.log "Missing WHEN annotation on field %s in union %s.\n"
        fld.fname fld.fcomp.cname;
      false
    end else
      (* Allow missing WHEN clauses for scalars. *)
      true

(* Determine whether a type is well-formed. *)
let rec checkType (ctx: context) (t: typ) : bool =
  let ctxThis = addThisBinding emptyContext zero in
  match t with
  | TPtr (bt, a) ->
      (* TODO: check whether base types for bounds match? *)
      checkType ctxThis bt &&
      (hasBindings ctx (depsOfAttrs a))
  | TArray (bt, _, _) ->
      checkType ctxThis bt
  | TFun (ret, argInfo, _, _) ->
      let ctxFun =
        List.fold_left
          (fun acc (name, _, _) -> addBinding acc name zero)
          ctxThis
          (argsToList argInfo)
      in
      checkType ctxThis ret &&
      List.fold_left
        (fun acc (_, t, _) -> acc && checkType ctxFun t)
        true
        (argsToList argInfo)
  | TComp (ci, _) when not ci.cstruct ->   (* union *)
      List.fold_left
        (fun acc fld -> 
           (* Check union fields in the context ["__this"; fieldname].
              These are redundant ... I'm only including the field
              name because that's how we did it in the paper. *)
           let ctxField = addBinding ctxThis fld.fname zero in
           if not (checkType ctxField fld.ftype) then
             E.s (error "Field %s of union %s is ill-formed"
                        fld.fname ci.cname);
           (* now check the when clause *)
           acc && (checkUnionWhen ctx fld) )
      true
      ci.cfields
      
  (* Structs and typedefs are checked when defined. *)
  | TComp _
  | TNamed _
  (* The following types are always well-formed. *)
  | TVoid _
  | TInt _
  | TFloat _
  | TEnum _
  | TBuiltin_va_list _ -> true

let polyTable : typ IH.t = IH.create 13

let rec checkPolyType (polyType: typ) (otherType: typ)
                      (e: exp) (forRead: bool) : unit =
  if isPoly otherType then begin
    if getPolyId polyType != getPolyId otherType then
      errorwarn "Cannot directly assign two different poly types"
  end else begin
    let n = getPolyId polyType in
    let instantiatedType =
      try
        IH.find polyTable n
      with Not_found ->
        let instantiatedType =
          typeAddAttributes [safeAttr]
            (typeRemoveAttributes ["fancybounds"] otherType)
        in
        IH.add polyTable n instantiatedType;
        instantiatedType
    in
    let fancyType =
      substType (addThisBinding emptyContext (CastE (instantiatedType, e)))
                instantiatedType
    in
    if forRead then
      coerceType e ~tfrom:fancyType ~tto:otherType
    else
      coerceType e ~tfrom:otherType ~tto:fancyType
  end

(* Add checks for a coercion of e from tfrom to tto.
   Both tfrom and tto must have fancy bounds. *)
and coerceType (e:exp) ~(tfrom : typ) ~(tto : typ) : unit =
  if !verbose then
    E.log "%a: coercing exp %a from %a to %a\n"
          d_loc !currentLoc dx_exp e dx_type tfrom dx_type tto;
  match unrollType tfrom, unrollType tto with
  | t1, t2 when isTrustedType t1 || isTrustedType t2 ->
      ()
  | (TInt _ | TPtr _), TPtr _ when isZero e ->
      (* Coerce NULL to pointer.  Do we need to do any well-formedness checks
         here? *)
      ()
  | (TPtr (TVoid _, _) as polyType), (TPtr _ as ptrType) when isPoly polyType ->
      checkPolyType polyType ptrType e true
  | (TPtr _ as ptrType), (TPtr (TVoid _, _) as polyType) when isPoly polyType ->
      checkPolyType polyType ptrType e false
  | TPtr(bt1, _), TPtr(bt2, _) when compareTypes bt1 bt2 ->
      if isNullterm tto && not (isNullterm tfrom) then
        errorwarn "Cast to NULLTERM from an ordinary pointer";
      let lo_from, hi_from = fancyBoundsOfType tfrom in
      let lo_to, hi_to = fancyBoundsOfType tto in
      if isNullterm tfrom then begin
        if bitsSizeOf bt2 <> 8 then
          E.s (unimp "nullterm buffer that's not a char*");
        if bitsSizeOf bt2 <> bitsSizeOf bt1 then
          (* this won't actually fail yet, since only char* NT is allowed *)
          E.s (error "NULLTERM cast between base types of different sizes");
        addCoercionCheck ~fromNullterm:true lo_from lo_to e hi_to hi_from bt2
      end else
        addCoercionCheck lo_from lo_to e hi_to hi_from bt2
  | TPtr (bt1, _), TPtr (bt2, _) when not (typeContainsPointers bt1) &&
                                      not (typeContainsPointers bt2) ->
      if isNullterm tto || isNullterm tfrom then
        E.s (unimp "Nullterm cast with different base types");
      let lo_from, hi_from = fancyBoundsOfType tfrom in
      let lo_to, hi_to = fancyBoundsOfType tto in
      addCoercionCheck lo_from lo_to e hi_to hi_from bt2
  | (TInt _ | TEnum _ | TPtr _), (TInt _ | TEnum _) ->
      (* These are all totally safe. *)
      ()
  | TComp (ci, _), TComp (ci', _) when ci == ci' && not ci.cstruct ->
      (* If the when maps differ, it's because a WHEN clause depends on
         something in the context that has changed, so we should ensurer the
         union has been zeroed.
         FIXME: using Ocaml's equality comparison may be too conservative. *)
      if not (Util.equals (fancyWhenOfType tfrom) (fancyWhenOfType tto)) then 
        begin
          let lv = match e with Lval lv -> lv 
            | _ -> E.s (bug "union expression must be an lval.")
          in
          addCheck (CNullUnion lv)
        end
  | _ -> 
    if not (compareTypes tfrom tto) then
      errorwarn "Type mismatch: coercion from %a to %a" dx_type tfrom dx_type tto
        
type whyLval=
    ForRead          (* Reading this lval. *)
  | ForAddrOf        (* Taking the address of this lval *)
  | ForWrite of exp  (* writing the specified value. Call checkExp on
                        this exp before calling checkLval *)
  | ForCall          (* Assigning the result of a call.
                        We don't have an expression representing the new value,
                        so we have to be more conservative *)

(* Calls checkExp e, then calls coerceType to make sure that
   e can be coerced to tto.  tto must have fancy bounds. *)
let rec coerceExp (e:exp) (tto : typ) : unit =
  (* If we're casting to a sentinel type, we do less-strict checking of e *)
  let toSentinel: bool = 
    (isPointerType tto) &&
    (let lo_to, hi_to = fancyBoundsOfType tto in
     compareExpStripCasts lo_to hi_to)
  in    
  let tfrom = checkExp ~toSentinel e in
  coerceType e ~tfrom ~tto
        

and checkExp ?(toSentinel=false) (e : exp) : typ =
  if !verbose then
    E.log "%a: checking exp %a\n" d_loc !currentLoc dx_exp e;
  match e with
  | UnOp (op, e', t) -> coerceExp e' t; t
  | BinOp ((PlusPI | IndexPI | MinusPI) as op, e1, e2, t) ->
      let t1 = checkExp e1 in
      (* FIXME: __this can appear in t, so we ignore it for now.
         At some point, we should check it! *)
      (* coerceExp e1 (substType ... t); *)
      coerceExp e2 intType;
      if not (isTrustedType t1) then begin
        let lo, hi = fancyBoundsOfType t1 in
        let e2' =
          match op with
          | MinusPI -> UnOp (Neg, e2, typeOf e2)
          | PlusPI | IndexPI -> e2
          | _ -> E.s (bug "Unexpected operation")
        in
        if toSentinel then 
          (* We're casting e to a sentinel.  We now permit sentinels to point
             to unallocated memory, so don't check that e1 is nonnull. *)
          addBoundsCheck lo e hi (isNullterm t1)
        else
          addArithChecks lo e1 e2' hi
      end;
      t1
  | BinOp (MinusPP, e1, e2, t) ->
      ignore (checkExp e1);
      ignore (checkExp e2);
      t
  | BinOp (op, e1, e2, t) ->
      coerceExp e1 t;
      coerceExp e2 t;
      t
  | Lval lv -> checkLval ForRead lv
  | CastE (t1, AddrOf (Mem (CastE (t2, z)), Field (f, NoOffset)))
        when isIntegralType t1 && isZero z ->
      t1
  | CastE (t, e') ->
      let ctx = addThisBinding (localsContext !curFunc) e in
      let t' = substType ctx t in
      coerceExp e' t';
      t'
  | SizeOfE _
  | AlignOfE _ ->
      (* We don't check the inner expr because it doesn't get executed. *)
      unrollType (typeOf e)
  | AddrOf lv ->
      ignore (checkLval ForAddrOf lv);
      let ctxThis = addThisBinding emptyContext zero in
      let bt = typeOfLval lv in
      if not (checkType ctxThis bt) then
        E.s (error "Cannot take address of lval that has dependencies");
      if hasExternalDeps lv then
        E.s (error "Cannot take address of lval with external dependencies");
      let lo = AddrOf lv in
      let hi = BinOp (PlusPI, lo, one, typeOf lo) in
      makeFancyPtrType bt lo hi
  | StartOf lv ->
      let bt, len, attrs =
        match unrollType (checkLval ForAddrOf lv) with
        | TArray (bt, Some e, attrs) ->
            let nt = hasAttribute "nullterm" attrs in
            let e' = if nt then BinOp (MinusA, e, one, typeOf e) else e in
            bt, e', attrs
        | TArray (bt, None, attrs) ->
            bt, zero, attrs
        | _ -> E.s (bug "Expected array type")
      in
      let lo = StartOf lv in
      let hi = BinOp (PlusPI, lo, len, typeOf lo) in
      typeAddAttributes (filterAttributes "nullterm" attrs)
                        (makeFancyPtrType bt lo hi)
  | Const (CStr s) -> (* String literal *)
      let len = String.length s in
      let lo = e in
      let hi = BinOp (PlusPI, lo, integer len, typeOf lo) in
      makeFancyPtrType ~nullterm:true charType lo hi
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> unrollType (typeOf e)

and checkLval (why: whyLval) (lv: lval) : typ =
  if !verbose then
    E.log "%a: checking lvalue %a\n" d_loc !currentLoc d_lval lv;
  begin
    match lv with
      Mem e, off ->
        let ptrTy = checkExp e in
        if not (isTrustedType ptrTy) then begin
          let lo, hi = fancyBoundsOfType ptrTy in
          addCheck (CNonNull e);
          let addUBoundChecks (): unit =
            addCheck (COverflow (e, one));
            let ePlusOne = BinOp(PlusPI, e, one, ptrTy) in
            addCheck (CUnsignedLE(ePlusOne,hi, "access"))
          in
          match why with
            ForRead ->
              if not (isNullterm ptrTy) then
                addUBoundChecks ()
          | ForAddrOf ->
              (* check e != hi even if this is nullterm, because
                 otherwise we could create a pointer with bounds hi,hi+1. *)
              addUBoundChecks ()
          | ForCall ->
            (* Conservatively forbid assignment of a call result when e=hi. *)
              addUBoundChecks ()
          | ForWrite what ->
              if isNullterm ptrTy then
                addCheck (CNTWrite(e,hi,what))
              else
                addUBoundChecks ()
      end
    | Var vi, off -> ()
  end;
  let lv', off = removeOffsetLval lv in
  let checkRest ():typ = (* returns the type of lv' *)
    let why' = match why with
        (* If why = ForWrite, then we are writing to a field or element of
           lv'. It doesn't make sense to say that we are writing "e" to the
           entire lval, so use the more conservative ForCall instead. *)
        ForWrite e -> ForCall
      | _ -> why
    in
    unrollType (checkLval why' lv')
  in
  match off with
  | NoOffset ->
      let ctx =
        match fst lv with
        | Var vi ->
            if not vi.vglob then
              localsContext !curFunc
            else if vi.vglob && vi.vstorage = Static then
              globalsContext ()
            else
              addBinding emptyContext vi.vname (Lval (var vi))
        | Mem e -> emptyContext
      in
      let ctx' = addThisBinding ctx (Lval lv) in
      substType ctx' (typeOfLval lv)
  | Field (fld, NoOffset) ->
      let compType = checkRest () in
      begin
        match compType with
        | TComp (ci, _) when ci == fld.fcomp -> ()
        | t ->
            E.s (error "Bad field offset %s on type %a" fld.fname dx_type t)
      end;
      if fld.fcomp.cstruct then begin
        let ctx = structContext lv' fld.fcomp in
        let ctx' = addThisBinding ctx (Lval lv) in
        substType ctx' fld.ftype
      end else begin (* Union *)
        (* check the field access *)
        if not (isTrustedComp fld.fcomp) then
          checkUnionAccess why compType fld;
        (* now do the type of the field itself *)
        let value = Lval lv in
        let ctx  = addBinding emptyContext fld.fname value in
        let ctx' = addThisBinding ctx value in
        substType ctx' fld.ftype
      end
  | Index (index, NoOffset) ->
      coerceExp index intType;
      begin
        match checkRest () with 
        | TArray (bt, Some len, a) ->
            addCheck (CUnsignedLess (index, len, "Array access"));
            if hasAttribute "nullterm" a then begin
              match why with
              | ForWrite what ->
                  let base = StartOf lv' in
                  let t = typeOf base in
                  let e = BinOp (PlusPI, base, index, t) in
                  let hi =
                    BinOp (MinusPI, BinOp (PlusPI, base, len, t), one, t)
                  in
                  addCheck (CNTWrite (e, hi, what))
              | _ -> ()
            end;
            let ctx = addThisBinding emptyContext (Lval lv) in
            substType ctx bt
        | TArray (bt, None, a) ->
            E.s (error "Accessing unsized array %a" d_lval lv')
        | t -> E.s (error "Expecting an array, got %a" dx_type t)
      end
  | _ -> E.s (bug "Unexpected result from removeOffset")

and checkUnionAccess (why:whyLval) (compType: typ) (fld:fieldinfo): unit =
  if (why = ForAddrOf) then
    E.s (error "Can't take the address of a union field");
  let wm = fancyWhenOfType compType in
  if !verbose then
    E.log "%a:  Read from %s.  Using fancywhen [%a]\n" 
      d_loc !currentLoc fld.fname d_whenMap wm;
  (* Check the selector for the current field. *)
  (try
     let s = List.assq fld wm in
     addCheck (CSelected s)
   with Not_found -> () (* a scalar field without a WHEN *)
  );
  if why <> ForRead then begin
    (* Check that the other selectors are 0 *)
    List.iter
      (fun (f,s) -> if f != fld then
         addCheck (CNotSelected s))
      wm
  end;  
  ()

let checkCall (lvo: lval option) (fnType: typ)
              (args: exp list) (exempt: int list) : unit =
  if !verbose then
    E.log "%a: checking call\n" d_loc !currentLoc;
  match fnType with
  | TFun (returnType, argInfo, varargs, _) ->
      if varargs then
        warn "Varargs were not checked";
      (match lvo with
       | Some lv ->
           if not (List.mem 0 exempt) then begin
             (* TODO: let the return type depend on formals *)
             if hasExternalDeps lv then
               E.s (error "Return lval has external dependencies");
             let lvType = checkLval ForCall lv in
             (* replace __this in the return type with lv, and make sure the
                result equals the type of lv: *)
             let returnCtx = addThisBinding emptyContext (Lval lv) in
             let returnType' = substType returnCtx returnType in
             (* TODO: the runtime checks emitted here may not be correct,
              * since we insert them before the call *)
             checkSameType returnType' lvType
           end
       | None -> ()
      );
      let formals = argsToList argInfo in
      let numFormals = List.length formals in
      let actuals1, actuals2 = split args numFormals in
      begin
        try
          let ctxCall =
            List.fold_left2
              (fun ctxAcc (argName, _, _) arg ->
                 if argName <> "" then
                   addBinding ctxAcc argName arg
                 else
                   ctxAcc)
              emptyContext
              formals
              actuals1
          in
          iter2index
            (fun (argName, argType, _) arg i ->
               if not (List.mem i exempt) then
                 let ctxCall' = addThisBinding ctxCall arg in
                 let argType' = substType ctxCall' argType in
                 coerceExp arg argType')
            formals
            actuals1
        with Invalid_argument _ ->
          E.s (bug "Different number of formal and actual args")
      end;
      iterindex
        (fun arg i ->
           if not (List.mem (i + numFormals) exempt) then
             ignore (checkExp arg))
        actuals2
  | _ -> E.log "%a: calling non-function type\n" d_loc !currentLoc

let checkAlloc (lvo: lval option) (fnType: typ) (args: exp list) : unit =
  (* Check all args, but don't check return value. Preprocessing has
   * ensured that the return value has the appropriate type. *)
  (* TODO: check anyway? *)
  checkCall lvo fnType args [0]

let checkMemset (lvo: lval option) (fnType: typ) (args: exp list) : unit =
  if !verbose then
    E.log "%a: checking memset\n" d_loc !currentLoc;
  let isCorrectSize (size: exp) (lv: lval) =
    (* return true if size is an expression for the size of lv. *)
    let actualSize : int = (bitsSizeOf (typeOfLval lv)) / 8 in
    let size' : int64 option = isInteger (constFold true size) in
    size' = Some (Int64.of_int actualSize)
  in
  match List.map stripOneCast args with
  | [AddrOf lv1; e2; e3] 
    when (isZero e2) && (isCorrectSize e3 lv1) ->
      (* Special case: if we're overwriting a complete lval with 0, we
         don't need to check for dependencies within lv1.  We still
         check to make sure nothing outside of lv1 depends on lv1.
         
         We need this when lv1 is a union.  It's okay to zero a union, but it's
         not normally okay to take the address of a union, since it
         depends on its context.
      *)
      ignore (checkLval ForAddrOf lv1);
      if hasExternalDeps lv1 then
        E.s (error
            "Memset: cannot take address of lval with external dependencies");
      checkCall lvo fnType [AddrOf lv1; e2; e3] [1]
  | [e1; e2; e3] ->
      let e1Type = checkExp e1 in
      let e1BaseType =
        match unrollType e1Type with
        | TPtr (bt, _) -> bt
        | _ -> E.s (error "First arg to memset is not a pointer")
      in
      let lo, hi = fancyBoundsOfType e1Type in
      let e1' = CastE (charPtrType, e1) in
      addArithChecks lo e1' e3 hi;
      if typeContainsPointers e1BaseType then begin
        addCheck (CEq (e2, zero));
        addCheck (CMult (SizeOf e1BaseType, e3))
      end;
      checkCall lvo fnType [e1; e2; e3] [1]
  | _ -> E.s (error "Expected three args to memset")

let checkMemcpy (lvo: lval option) (fnType: typ) (args: exp list) : unit =
  if !verbose then
    E.log "%a: checking memcpy\n" d_loc !currentLoc;
  match args with
  | [e1; e2; e3] ->
      let e1 = stripOneCast e1 in
      let e2 = stripOneCast e2 in
      let e1Type = checkExp e1 in
      let e2Type = checkExp e2 in
      let e1BaseType =
        match unrollType e1Type with
        | TPtr (bt, _) -> bt
        | _ -> E.s (error "First arg to memcpy is not a pointer")
      in
      let e2BaseType =
        match unrollType e2Type with
        | TPtr (bt, _) -> bt
        | _ -> E.s (error "Second arg to memcpy is not a pointer")
      in
      let lo1, hi1 = fancyBoundsOfType e1Type in
      let e1' = CastE (charPtrType, e1) in
      addArithChecks lo1 e1' e3 hi1;
      let lo2, hi2 = fancyBoundsOfType e2Type in
      let e2' = CastE (charPtrType, e2) in
      addArithChecks lo2 e2' e3 hi2;
      if typeContainsPointers e1BaseType then begin
        checkSameType e1BaseType e2BaseType;
        addCheck (CMult (SizeOf e1BaseType, e3))
      end;
      checkCall lvo fnType [e1; e2; e3] [1; 2]
  | _ -> E.s (error "Expected three args to memcpy")

let checkMemcmp (lvo: lval option) (fnType: typ) (args: exp list) : unit =
  if !verbose then
    E.log "%a: checking memcmp\n" d_loc !currentLoc;
  match args with
  | [e1; e2; e3] ->
      let e1 = stripOneCast e1 in
      let e2 = stripOneCast e2 in
      let e1Type = checkExp e1 in
      let e2Type = checkExp e2 in
      let lo1, hi1 = fancyBoundsOfType e1Type in
      let e1' = CastE (charPtrType, e1) in
      addArithChecks lo1 e1' e3 hi1;
      let lo2, hi2 = fancyBoundsOfType e2Type in
      let e2' = CastE (charPtrType, e2) in
      addArithChecks lo2 e2' e3 hi2;
      checkCall lvo fnType [e1; e2; e3] [1; 2]
  | _ -> E.s (error "Expected three args to memcmp")


let checkSetEnv (ctx: context) (x: 'a) (e: exp) (env: 'a list) (expOf: 'a -> exp)
                (nameOf: 'a -> string) (typeOf: 'a -> typ) : unit =
  (* Cast e to its new type, so that we do arithmetic correctly. *)
  let eCast = mkCast ~e ~newt:(typeOf x) in
  List.iter
    (fun y ->
       let yExp = expOf y in
       (* ySubst is the new value of y after the assignment.  
          ySubstCast is ySubst with a cast to its new type, for use in
          the enviroment.  Without the cast, case cast3 of cast9.c incorrectly
          passes because the arithmetic is wrong.  *)
       let ySubst, ySubstCast =  if (nameOf x) <> (nameOf y) 
                                 then  yExp, yExp  else  e, eCast
       in
       let ctx' = addBinding 
                    (addThisBinding ctx ySubstCast) 
                    (nameOf x) 
                    eCast in
       coerceExp ySubst (substType ctx' (typeOf y)))
    env

let checkSet (lv: lval) (e: exp) : unit =
  (* log "checkSet for %a := %a\n" d_lval lv dx_exp e; *)
  ignore (checkLval (ForWrite e) lv);
  let off1, off2 = removeOffset (snd lv) in
  begin
    match off2 with
    | NoOffset ->
        begin
          match fst lv with
          | Var x ->
              let ctx, env =
                if not x.vglob then
                  localsContext !curFunc, !curFunc.slocals @ !curFunc.sformals
                else if x.vglob && x.vstorage = Static then
                  globalsContext (), !staticGlobalVars
                else
                  addBinding emptyContext x.vname (Lval (var x)), [x]
              in
              checkSetEnv ctx x e env
                       (fun vi -> Lval (var vi))
                       (fun vi -> vi.vname)
                       (fun vi -> vi.vtype)
          | Mem addr ->
              let ctx = addThisBinding emptyContext e in
              coerceExp e (substType ctx (typeOfLval lv))
        end
    | Field (x, NoOffset) when x.fcomp.cstruct -> (* struct *)
        let baseLval = fst lv, off1 in
        let ctx = structContext baseLval x.fcomp in
        let env = x.fcomp.cfields in
        checkSetEnv ctx x e env
                 (fun fi -> Lval (addOffsetLval (Field (fi, NoOffset)) baseLval))
                 (fun fi -> fi.fname)
                 (fun fi -> fi.ftype)
    | Field (x, NoOffset) ->   (* Union *)
        (* union fields don't depend on each other. *)
        ()
    | Index (_, NoOffset) ->
        (* No dependencies to array elements. 
           FIXME: what about arrays inside null-terminated arrays?  *)
        ()
    | _ -> E.s (bug "Unexpected result from removeOffset")
  end

let checkInstr (instr : instr) : unit =
  currentLoc := get_instrLoc instr;
  if !verbose then
    E.log "%a: checking instr %a\n" d_loc !currentLoc d_instr instr;
  match instr with
  | Call (lvo, fn, args, _) ->
      let fnType = checkExp fn in
      if isAllocator fnType then
        checkAlloc lvo fnType args
      else if isMemset fnType then
        checkMemset lvo fnType args
      else if isMemcpy fnType then
        checkMemcpy lvo fnType args
      else if isMemcmp fnType then
        checkMemcmp lvo fnType args
      else
        checkCall lvo fnType args []
  | Set ((Var vi, NoOffset), _, _) when List.memq vi !exemptLocalVars ->
      ()
  | Set (lv, e, _) ->
      checkSet lv e
  | Asm _ ->
      warn "Ignoring asm"

let checkReturn (eo : exp option) : unit =
  let returnType =
    match !curFunc.svar.vtype with
    | TFun (returnType, _, _, _) -> returnType
    | _ -> E.s (bug "Expected function type")
  in
  match eo with
  | Some e ->
      if !verbose then
        E.log "%a: checking return %a\n" d_loc !currentLoc dx_exp e;
      let ctx = addThisBinding emptyContext e in
      coerceExp e (substType ctx returnType)
  | None ->
      if !verbose then
        E.log "%a: checking return\n" d_loc !currentLoc;
      checkSameType returnType voidType

let rec checkStmt (s : stmt) : unit =
  curStmt := s.sid;
  currentLoc := get_stmtLoc s.skind;
  match s.skind with
  | Instr instrs ->
      List.iter checkInstr instrs
  | Return (eo, _) ->
      checkReturn eo
  | If (e, b1, b2, _) ->
      coerceExp e intType;
      checkBlock b1;
      checkBlock b2;
  | Switch (e, b, _, _) ->
      coerceExp e intType;
      checkBlock b
  | Loop (b, _, _, _)
  | Block b -> checkBlock b
  | Goto _
  | Break _
  | Continue _ -> ()
  | TryFinally _
  | TryExcept _ -> E.s (E.unimp "exceptions not supported\n")

and checkBlock (b : block) : unit =
  List.iter checkStmt b.bstmts

let checkTypedef (ti: typeinfo) : unit =
  let ctxThis = addThisBinding emptyContext zero in
  if not (checkType ctxThis ti.ttype) then
    E.s (error "Type of typedef %s is ill-formed" ti.tname)

let checkStruct (ci: compinfo) : unit =
  let ctx =
    List.fold_left
      (fun acc fld -> addBinding acc fld.fname zero)
      (addThisBinding emptyContext zero)
      ci.cfields
  in
  List.iter
    (fun fld ->
       if not (checkType ctx fld.ftype) then
         E.s (error "Field %s of struct %s is ill-formed" fld.fname ci.cname))
    ci.cfields

let checkVar (vi: varinfo) (init: initinfo) : unit =
  let ctxThis = addThisBinding (globalsContext ()) zero in
  if not (checkType ctxThis vi.vtype) then
    E.s (error "Type of global %s is ill-formed" vi.vname);
  if init.init <> None then
    warn "Global variable initializer was not checked"

let makeCFG (fd : fundec) : unit =
  let cnt = Cfg.cfgFun fd in
  Cfg.start_id := cnt + !Cfg.start_id


let checkFundec (fd : fundec) (loc:location) : unit =
  if !verbose then
    E.log "Doing function %s.\n" fd.svar.vname;
  curFunc := fd;
  clearBoundsTable ();
  let ctx = localsContext fd in
  let ctxThis = addThisBinding ctx zero in
  List.iter
    (fun vi ->
       if not (checkType ctxThis vi.vtype) then
         E.s (error "Type of variable %s is ill-formed" vi.vname))
    (fd.slocals @ fd.sformals);
  (* fix block, see if cfg should be used, check block *)
  fixBlock fd.sbody;
  if !optLevel >= 2 then makeCFG fd;
  checkBlock fd.sbody;
  curFunc := dummyFunDec;
  curStmt := -1;
  (* Initialize all locals to 0.  Do this after adding checks *)
  let init: instr list =
    List.map
      (fun vi ->
         let t = unrollType vi.vtype in
         match t with
           TInt _
         | TEnum _
         | TPtr _ ->
             Set(var vi, zero, loc)
         | TFloat _ -> 
             Set(var vi, Const(CReal(0.0, FFloat, None)), loc)
         | TComp _ 
         | TArray _ ->
             Call(None, memset,
                  [mkAddrOf (var vi); zero; SizeOf t],
                  loc)
         | _ -> E.s(bug "Unexpected type %a for local var %s." 
                      dx_type t vi.vname))
      fd.slocals
  in
  let init' = mkStmt(Instr init) in
  assignID init';
  (* zra - need CFG info for stmts added after makeCFG *)
  if !optLevel >= 2 then begin
    (init'.sid <- !Cfg.start_id + 1; incr Cfg.start_id);
    init'.succs <- [List.hd fd.sbody.bstmts];
  end;
  fd.sbody.bstmts <- init'::fd.sbody.bstmts;
  ()

(**************************************************************************)

let inferVisitor = object (self)
  inherit nopCilVisitor

  val varBounds : (string, varinfo * varinfo) Hashtbl.t =
    Hashtbl.create 7

  val mutable curIndex = 0

  method private makeName (base: string) =
    curIndex <- curIndex + 1;
    base ^ (string_of_int curIndex)

  method private needsAnnot (t: typ) =
    isPointerType t && not (hasAttribute "bounds" (typeAttrs t))

  method private makeBoundVars (name: string) (bt: typ) (a: attributes) =
    let makeBoundVar (suffix: string) : varinfo =
      let boundName = name ^ suffix in
      let boundType = TPtr (bt, sentinelAttr :: a) in
      makeLocalVar !curFunc boundName boundType
    in
    let baseVar = makeBoundVar "__b" in
    let endVar = makeBoundVar "__e" in
    let boundAttr =
      Attr ("bounds", [ACons (baseVar.vname, []);
                       ACons (endVar.vname, [])])
    in
    baseVar, endVar, boundAttr

  method private getPointerBounds (toType: typ) (fromType: typ) (e: exp) =
    let lo, hi = fancyBoundsOfType fromType in
    if isNullterm fromType && not (isNullterm toType) then
      let tmp = makeLocalVar !curFunc (self#makeName "depstrlen") intType in
      let instrs = [Call (Some (var tmp), strlen, [hi], !currentLoc)] in
      let hi' = BinOp (PlusPI, hi, Lval (var tmp), typeOf hi) in
      instrs, lo, hi'
    else if isPoly fromType then
      let instantiatedType =
        typeAddAttributes [safeAttr]
          (typeRemoveAttributes ["bounds"] toType)
      in
      let fancyType =
        substType (addThisBinding emptyContext (CastE (instantiatedType, e)))
                  instantiatedType
      in
      let lo', hi' = fancyBoundsOfType fancyType in
      [], lo', hi'
    else
      [], lo, hi

  method vtype t =
    let postProcessType (t: typ) =
      if self#needsAnnot t then
        typeAddAttributes [safeAttr] t
      else
        t
    in
    ChangeDoChildrenPost (t, postProcessType)

  method vexpr e =
    let castNeedsAnnot =
      match e with
      | CastE (t, _) when self#needsAnnot t -> true
      | _ -> false
    in
    let postProcessExpr (e: exp) =
      (* matt: we'll have to do something else for casts in globals,
         since inserting variables won't help. *)
      if castNeedsAnnot && (!curFunc != dummyFunDec) then
        match e with
        | CastE (t, e') ->
            if !verbose then
              log "inferring for cast %a\n" dn_exp e;
            let bt, a =
              match unrollType t with
              | TPtr (bt, a) -> bt, dropAttribute "bounds" a
              | _ -> E.s (bug "expected pointer type")
            in
            let baseVar, endVar, boundAttr =
              self#makeBoundVars (self#makeName "cbound") bt a
            in
	    let newInstrs, lo, hi =
	      if isZero e then
	        [], zero, zero
              else begin
	        let t' = checkExp e' in
                if isPointerType t' then
                  self#getPointerBounds t t' e'
                else
                  [], e', e'
	      end
	    in
	    let lo', hi' =
	      CastE (baseVar.vtype, lo), CastE (endVar.vtype, hi)
	    in
            self#queueInstr newInstrs;
	    self#queueInstr [Set (var baseVar, lo', !currentLoc);
                             Set (var endVar, hi', !currentLoc)];
            CastE (TPtr (bt, boundAttr :: a), e')
        | _ -> E.s (bug "expected cast expression")
      else
        e
    in
    ChangeDoChildrenPost (e, postProcessExpr)

  method vinst i =
    (* Process individual instructions.   *)
    let postProcessInstr (instrs: instr list) : instr list =
      match instrs with 
        [] -> []
      | [Set ((Var vi, NoOffset), e, l) as instr]
          when Hashtbl.mem varBounds vi.vname ->
            if !verbose then
              log "inferring for instr %a\n" dn_instr instr;
            let baseVar, endVar = Hashtbl.find varBounds vi.vname in
            let t = checkExp e in
            (* matth: don't insert an If that checks whether vi is null.
               This means that sentinels may point to unmapped memory.*)
            if isZero e then
              [Set (var vi, zero, l);
               Set (var baseVar, zero, l);
               Set (var endVar, zero, l)]
            else begin
              let newInstrs, lo, hi =
                if isPointerType t then begin
                  self#getPointerBounds vi.vtype t e
                end else
                  [], Lval (var vi), Lval (var vi)
              in
              newInstrs @
              [Set (var vi, zero, l);
               Set (var baseVar, lo, l);
               Set (var endVar, hi, l);
               instr]
            end
      | [_] -> instrs
      | _ -> 
          E.s (bug "more than one instr in inferVisitor:postProcessInstr.")
    in
    ChangeDoChildrenPost([i], postProcessInstr)
          

  method vfunc fd =
    Hashtbl.clear varBounds;
    curFunc := fd;
    List.iter
      (fun vi ->
         match vi.vtype with
         | TPtr (bt, a) when not (hasAttribute "bounds" a) ->
	     let baseVar, endVar, boundAttr =
               self#makeBoundVars vi.vname bt a
             in
             vi.vtype <- TPtr (bt, boundAttr :: a);
             Hashtbl.add varBounds vi.vname (baseVar, endVar)
         | _ -> ())
      fd.slocals;
    let cleanup x =
      Hashtbl.clear varBounds;
      curFunc := dummyFunDec;
      x
    in
    ChangeDoChildrenPost (fd, cleanup)

end

(**************************************************************************)

let stripSomeCasts (t: typ) (e: exp) : exp =
  match e with
  | CastE (t', e') ->
      let normalize =
        typeRemoveAttributes ["bounds"; "nullterm"; "trusted"; "poly"]
      in
      if compareTypes t t' &&
         (compareTypes (normalize t') (normalize (typeOf e')) || 
          isPoly t' || (isPointerType t' && isZero e)) then
        e'
      else
        e
  | _ -> e

let rec expRefersToVar (name: string) (e: exp) : bool =
  match e with
  | Lval lv -> lvalRefersToVar name lv
  | AddrOf lv -> lvalRefersToVar name lv
  | StartOf lv -> lvalRefersToVar name lv
  | SizeOfE e' -> expRefersToVar name e'
  | AlignOfE e' -> expRefersToVar name e'
  | UnOp (_, e', _) -> expRefersToVar name e'
  | BinOp (_, e1, e2, _) -> expRefersToVar name e1 || expRefersToVar name e2
  | CastE (_, e') -> expRefersToVar name e'
  | Const _
  | SizeOf _
  | SizeOfStr _
  | AlignOf _ -> false

and lvalRefersToVar (name: string) ((host, offset): lval) : bool =
  let rec offsetRefersToVar (offset: offset) =
    match offset with
    | Field (fld, offset') -> offsetRefersToVar offset'
    | Index (e, offset') -> expRefersToVar name e || offsetRefersToVar offset'
    | NoOffset -> false
  in
  match host with
  | Var vi -> vi.vname = name || offsetRefersToVar offset
  | Mem e -> expRefersToVar name e || offsetRefersToVar offset

let preProcessVisitor = object (self)
  inherit nopCilVisitor

  method vexpr e =
    match e with
    | Const (CStr str) when !curFunc != dummyFunDec ->
        let t =
          TPtr (charType, [Attr ("nullterm", []);
                           Attr ("bounds", [ACons ("__this", []);
                                            ACons ("__this", [])])])
        in
        let tmp = makeTempVar !curFunc t in
        exemptLocalVars := tmp :: !exemptLocalVars;
        self#queueInstr [Set (var tmp, e, locUnknown)];
        ChangeTo (Lval (var tmp))
    | _ -> DoChildren

  method vinst i = 
    let postProcessInstr (instrs: instr list) : instr list =
      List.fold_right
        (fun instr acc ->
           let stripArgCasts argInfo args =
             let formals = List.map (fun (_, t, _) -> t) (argsToList argInfo) in
             let actuals1, actuals2 = split args (List.length formals) in
             try
               (List.map2 stripSomeCasts formals actuals1) @ actuals2
             with Invalid_argument "List.map2" ->
               E.s (bug "Expected lists of equal length")
           in
           match instr with
           | Call (ret, fn, args, l) when isAllocator (typeOf fn) ->
               let argInfo =
                 match typeOf fn with
                 | TFun (_, argInfo, _, _) -> argInfo
                 | _ -> E.s (bug "Expected function type")
               in
               let lv =
                 match ret with
                 | Some lv -> lv
                 | None -> E.s (error "Allocation has no return")
               in
               let t = getAllocationType (typeOfLval lv) (typeOf fn) args in
               let tmp = makeTempVar !curFunc t in
               Call (Some (var tmp), fn, stripArgCasts argInfo args, l) ::
                 Set (lv, Lval (var tmp), l) ::
                 Set (var tmp, zero, l) ::
                 acc
           | Call (ret, fn, args, l) when isMemset (typeOf fn) ||
                                          isMemcpy (typeOf fn) ||
                                          isMemcmp (typeOf fn) ->
               Call (ret, fn, List.map stripOneCast args, l) :: acc
           | Call (Some (Var vi, NoOffset), fn, args, l) ->
               let rt, argInfo =
                 match typeOf fn with
                 | TFun (rt, argInfo, _, _) ->
                     if isPointerType rt &&
                        not (hasAttribute "bounds" (typeAttrs rt)) then
                       typeAddAttributes [safeAttr] rt, argInfo
                     else
                       rt, argInfo
                 | _ ->
                     E.s (bug "Expected function type")
               in
               let tmp = makeTempVar !curFunc rt in
               Call (Some (var tmp), fn, stripArgCasts argInfo args, l) ::
                 Set (var vi, Lval (var tmp), l) ::
                 Set (var tmp, zero, l) ::
                 acc
           | Call (lvo, fn, args, l) ->
               let argInfo =
                 match typeOf fn with
                 | TFun (_, argInfo, _, _) -> argInfo
                 | _ -> E.s (bug "Expected function type")
               in
               Call (lvo, fn, stripArgCasts argInfo args, l) :: acc
           | Set ((Var vi, NoOffset), e, l) when expRefersToVar vi.vname e ->
               let e' = stripSomeCasts vi.vtype e in
               let t = typeOf e' in
               let tmp = makeTempVar !curFunc t in
               Set (var tmp, e', l) ::
                 Set (var vi, Lval (var tmp), l) ::
                 Set (var tmp, zero, l) ::
                 acc
           | Set (lv, e, l) ->
               Set (lv, stripSomeCasts (typeOfLval lv) e, l) :: acc
           | _ ->
               instr :: acc)
        instrs
        []
    in
    ChangeDoChildrenPost ([i], postProcessInstr)

  method vstmt s =
    let postProcessStmt s =
      begin
        match s.skind with
        | Return (Some e, l) ->
            let returnType =
              match !curFunc.svar.vtype with
              | TFun (returnType, _, _, _) -> returnType
              | _ -> E.s (bug "Expected function type")
            in
            s.skind <- Return (Some (stripSomeCasts returnType e), l)
        | _ -> ()
      end;
      s
    in
    ChangeDoChildrenPost (s, postProcessStmt)

  method vfunc fd =
    curFunc := fd;
    let cleanup x =
      curFunc := dummyFunDec;
      x
    in
    ChangeDoChildrenPost (fd, cleanup)
end

(**************************************************************************)

let polyVisitor = object (self)
  inherit nopCilVisitor

  val mutable curPolyId = 0

  method private makePolyId () =
    curPolyId <- curPolyId + 1;
    curPolyId

  method private assignPolyId t =
    if isPoly t && not (hasPolyId t) then
      typeAddAttributes
        [Attr ("poly", [AInt (self#makePolyId ())])]
        (typeRemoveAttributes ["poly"] t)
    else
      t

  method private updateFunctionType t =
    match t with
    | TFun (ret, args, vararg, attrs) ->
        let ret' = self#assignPolyId ret in
        let args' =
          match args with
          | Some args ->
              let args' =
                List.map
                  (fun (argName, argType, argAttrs) ->
                     argName, self#assignPolyId argType, argAttrs)
                  args
              in
              Some args'
          | None -> None
        in
        TFun (ret', args', vararg, attrs)
    | _ -> t

  method vglob g =
    begin
      match g with
      | GVar (vi, _, _)
      | GVarDecl (vi, _) ->
          if isFunctionType vi.vtype then
            vi.vtype <- self#updateFunctionType vi.vtype
          else
            vi.vtype <- self#assignPolyId vi.vtype
      | GFun (fd, _) ->
          setFunctionType fd (self#updateFunctionType fd.svar.vtype);
          List.iter (fun vi -> vi.vtype <- self#assignPolyId vi.vtype)
                    fd.slocals;
      | GCompTag (ci, _) ->
          List.iter (fun fi -> fi.ftype <- self#assignPolyId fi.ftype)
                    ci.cfields
      | _ -> ()
    end;
    SkipChildren
end

(**************************************************************************)

(* Returns the size of a pointer's base type in bytes, if known *)
let sizeOfBaseType ptrt: int option =
  match unrollType ptrt with
  | TPtr (bt, _) -> begin
      match isInteger (constFold true (SizeOf bt)) with
      | Some n -> Some (to_int n)
      | None -> None
    end
  | _ -> (* maybe the expression is NULL *)
      None


(* Do we need an alignment check for p + x?  Well, that depends on the size of
 *  *p.  If the size is a power of two, p + x will be aligned even if it
 *  overflows, so we can skip the check. *)
let needsAlignCheck ptrt: bool =
  match sizeOfBaseType ptrt with (* Look for common multiples of 2 *)
    Some (1|2|4|8|16|32|64|128|256|512|1024|2048|4096) -> false
  | _ -> true

let rec getBaseOffset (e: exp) : exp * int =
  match e with
  | BinOp ((PlusPI | IndexPI | MinusPI) as op, e', off, t) ->
      let intFold e = isInteger (constFold true e) in
      begin
        match getBaseOffset e', intFold off, op with
        | (b, n1), Some n2, (PlusPI | IndexPI) ->
            b, n1 + (to_int n2)
        | (b, n1), Some n2, MinusPI ->
            b, n1 - (to_int n2)
        | _, _, _ ->
            e, 0
      end
  | _ ->
      e, 0


let proveLeWithBounds (e1: exp) (e2: exp) : bool =
  let ctx = allContext () in
  let rec getExpBounds (e:exp) : exp option * exp option =
    match e with
      (* TODO: structs, memory *)
    | Lval (Var vi, NoOffset) when isPointerType vi.vtype ->
        let ctx = addThisBinding ctx e in
        let lo, hi = boundsOfAttrs ctx (typeAttrs vi.vtype) in
(*         log " %a has bounds %a and %a.\n" dx_exp e *)
(*           dx_exp lo dx_exp hi; *)
        Some lo, Some hi
    | CastE (_, e') -> getExpBounds e'
    | _ -> None, None
  in
  let lo1, hi1 = getExpBounds e1 in
  let lo2, hi2 = getExpBounds e2 in
  (* we know e1 <= hi1 and lo2 <= e2 *)
  match hi1, lo2 with
    Some hi1, Some lo2 ->
      (compareExpStripCasts hi1 lo2)
      || (compareExpStripCasts hi1 e2)
      || (compareExpStripCasts e1 lo2)
  | Some hi1, None ->
      (compareExpStripCasts hi1 e2)
  | None, Some lo2 ->
      (compareExpStripCasts e1 lo2)
  | None, None ->
      false

let proveLe ?(allowGt: bool = false) (e1: exp) (e2: exp) : bool =
  let b1, off1 = getBaseOffset (stripNopCasts e1) in
  let b2, off2 = getBaseOffset (stripNopCasts e2) in
(*   log "  Comparing:\n"; *)
(*   log "   %a = (%a) + %d.\n" dx_exp e1 d_plainexp b1 off1; *)
(*   log "   %a = (%a) + %d.\n" dx_exp e2 d_plainexp b2 off2; *)
  if compareExpStripCasts b1 b2 then begin
    let doCompare n1 n2 =
      if n1 > n2 then begin
        if not allowGt then
          error "Bounds check (%a <= %a) will always fail" dx_exp e1 dx_exp e2;
        false
      end else begin
        true
      end
    in
    let t1 = typeOf b1 in
    let t2 = typeOf b2 in
    match sizeOfBaseType t1, sizeOfBaseType t2 with
    | Some n1, Some n2 -> doCompare (off1 * n1) (off2 * n2)
    | _ when compareTypes t1 t2 -> doCompare off1 off2
    | _ -> 
(*         log "   Comparetypes %a, %a failed.\n" dx_type t1 dx_type t2; *)
        false
  end else begin
    (proveLeWithBounds b1 b2 && off1 = 0 && off2 = 0)
  end

let optimizeCheck (c: check) : check list =
  match c with
  | CNullOrLE (_, e1, e2, _)
  | CUnsignedLE (e1, e2, _) ->
      (* If e1 is statically larger than e2, this test could still pass
         if e == 0.  But Report an error, since it's probably a coding
         bug anyways. *)
      if proveLe e1 e2 then
        []
      else
        [c]
  | CNullOrLENT (_, e1, e2, _)
  | CUnsignedLENT (e1, e2, _) ->
      if proveLe ~allowGt:true e1 e2 then
        []
      else
        [c]
  | CEq(e1, e2) -> 
      if compareExpStripCasts e1 e2 then []
      else begin
        warn "CEq: Couldn't prove %a  ==  %a. Inserting a runtime check.\n"
          d_plainexp e1 d_plainexp e2;
        [c]
      end
  | COverflow(ptr, Const(CInt64 (off,_,_))) ->
      let off = to_int off in
   (* matth:  We'll assume that the top page of memory is unmapped.
      Therefore, if ptr is in bounds, we can assume ptr < (2^32 - 4096),
      and ptr+off will not overflow for small values of off.
      FIXME: does kmalloc actually obey this assumption?  *)
      let sizeOfBase = match sizeOfBaseType (typeOf ptr) with
          Some s -> s
        | None -> E.s (error ("Overflow check on %a + %d, but I don't know"
                              ^^" how big each element is.\n") dx_exp ptr off)
      in
      if (off >= 0) && (off*sizeOfBase < 4096) then []
      else [c]

  | _ -> [c]

let optimizeVisitor = object (self)
  inherit nopCilVisitor

  method vstmt s =
    let checks = GA.getg allChecks s.sid in
    GA.setg allChecks s.sid (List.flatten (List.map optimizeCheck checks));
    DoChildren

  method vfunc fd =
    curFunc := fd;
    let cleanup x =
      curFunc := dummyFunDec;
      x
    in
    ChangeDoChildrenPost (fd, cleanup)

end

(* map f to all the expressions in a check *)
(* (exp -> exp) -> check -> check *)
let map_to_check f c =
    match c with
      CNonNull e -> 
	let e' = f e in
	CNonNull e'
    | CEq(e1,e2) ->
	let e1' = f e1 in
	let e2' = f e2 in
	CEq(e1',e2')
    | CPositive e ->
	let e' = f e in
	CPositive(e')
    | CMult(e1,e2) ->
	let e1' = f e1 in
	let e2' = f e2 in
	CMult(e1',e2')
    | COverflow(e1,e2) ->
	let e1' = f e1 in
	let e2' = f e2 in
	COverflow(e1',e2')
    | CUnsignedLess(e1,e2,why) ->
	let e1' = f e1 in
	let e2' = f e2 in
	CUnsignedLess(e1',e2',why)
    | CUnsignedLE(e1,e2,why) ->
	let e1' = f e1 in
	let e2' = f e2 in
	CUnsignedLE(e1',e2',why)
    | CUnsignedLENT(e1,e2,why) ->
	let e1' = f e1 in
	let e2' = f e2 in
	CUnsignedLENT(e1',e2',why)
    | CNullOrLE(e1,e2,e3,why) ->
	let e1' = f e1 in
	let e2' = f e2 in
	let e3' = f e3 in
	CNullOrLE(e1',e2',e3',why)
    | CNullOrLENT(e1,e2,e3,why) ->
	let e1' = f e1 in
	let e2' = f e2 in
	let e3' = f e3 in
	CNullOrLENT(e1',e2',e3',why)
    | CNTWrite(e1,e2,e3) ->
	let e1' = f e1 in
	let e2' = f e2 in
	let e3' = f e3 in
	CNTWrite(e1',e2',e3')
    | CSelected e ->
	let e' = f e in
	CSelected e'
    | CNotSelected e ->
	let e' = f e in
	CNotSelected e'
    | CNullUnion _ -> c

(* Applies action to all expressions in a function.
 * action takes reaching definition data, an expression,
 * the fundec that the expression is in, and a boolean.
 * If the boolean is true then all variables are considered.
 * If the boolean is false then only temps are considered. 
 * The returned bool is true if the expression was changed *)
(* action: RD.IOS.t IH.t -> exp -> fundec -> bool -> exp * bool *)
let checkVisit_change = ref false
let checkVisit action (fd : fundec) = object(self)
    inherit RD.rdVisitorClass

  method private do_action b e =
    match self#get_cur_iosh() with
      None -> e
    | Some iosh -> 
	let e', b' = action iosh e fd b in
	if b' then checkVisit_change := true;
	e'

  method private fix_check =
    map_to_check (self#do_action true)

  method private fix_checks cl =
    List.map self#fix_check cl

  val mutable check_sid = -1

  method private handle_checks () =
    if sid = check_sid then () else
    let checks = GA.getg allChecks sid in
    GA.set allChecks sid (self#fix_checks checks);
    check_sid <- sid

  method vexpr e = 
    self#handle_checks();
    ChangeTo(self#do_action false e)

  method vfunc fd =
    RD.computeRDs fd;  
    DoChildren

end

(* applies the action to the function until
   no changes are made *)
(* action: RD.IOS.t IH.t -> exp -> fundec -> bool -> exp * bool *)
(* action -> fundec -> unit *)
let fp action fd =
  let vis = checkVisit action fd in
  let rec loop () =
    checkVisit_change := false;
    ignore(visitCilFunction (vis :> cilVisitor) fd);
    if !checkVisit_change
    then loop() else ()
  in
  loop()

(* zra - Try to remove tmp variables in exps
 * through forward substitution.
 *
 * This also does forward subst. of everything
 * into checks, but this should probably be replaced
 * by an accurate constant propagation.
 *
 * Run after optimizeVisitor.
 * Only run if optLevel = 2 
 * Temps might be copied to temps, so it is necessary
 * to reach a fixed point.
 *)
let forwardTmpSub = fp RCT.fwd_subst

(* Constant propagation into checks *)
let constProp = fp RCT.const_prop

(**************************************************************************)
(**************************************************************************)
(* new, flow-sensitive optimizer *)

type symval = {
  v: varinfo;
}
type term = exp
(*     TVar of varinfo *)
(*   | TPlusPI of varinfo * exp *)
let compareTerm = compareExpStripCasts
    
module VarMap = Map.Make(struct 
                           type t = int
                           let compare i1 i2 = i2 - i1
                         end)
type absState = {
  varstate: term VarMap.t;
  nonNullVars: varinfo list;
(*   disEqual: (varinfo * varinfo) list; *)
}
let top = { varstate = VarMap.empty; nonNullVars = []; }
let d_state () a: doc =
  dprintf "Nonnull vars:@[%a@]"
    (docList (fun vi -> text vi.vname)) a.nonNullVars
(*   dprintf "Nonnull vars:@[%a@].  Disequal vars:@[%a@]"  *)
(*     (docList (fun vi -> text vi.vname)) a.nonNullVars *)
(*     (docList (fun (v1,v2) -> text v1.vname ++ text " != " ++ text v2.vname)) *)
(*       a.disEqual *)

let addNonNull (a:absState) (vi: varinfo) : absState = 
  if List.mem vi a.nonNullVars then a
  else begin
(*     log "%s is nonnull.\n" vi.vname; *)
    { a with nonNullVars = vi::a.nonNullVars }
  end
(* let areDisequal (a:absState) (v1: varinfo) (v2: varinfo): bool =  *)
(*   List.exists  *)
(*     (fun (v1',v2') -> (v1==v1'&&v2==v2') || (v2==v1'&&v1==v2')) *)
(*     a.disEqual *)

(* let addDisEqual (a:absState) (v1: varinfo) (v2: varinfo) : absState =  *)
(*   if areDisequal a v1 v2 then a *)
(*   else begin *)
(*     { a with disEqual = (v1,v2)::a.disEqual } *)
(*   end *)


let scrambleVar (a:absState) (v: varinfo) : absState =
  let a =
    if List.memq v a.nonNullVars then
      { a with nonNullVars = List.filter ((!=) v) a.nonNullVars }
    else
      a
  in
(*   let doesNotReferToV (v1',v2') = (v==v1') || (v==v2')) in *)
(*   if List.for_all doesNotReferToV a.disEqual then a *)
(*   else *)
(*     { a with disEqual = List.filter doesNotReferToV a.disEqual } *)
  a  
  
let stateMap : absState IH.t = IH.create 50

let isNonNull state e: bool = 
(*   log "isNonNull? on %a.\n" d_plainexp e; *)
  match stripNopCasts e with
    Lval(Var vi, NoOffset) -> 
      List.memq vi state.nonNullVars 
  | BinOp((PlusPI|IndexPI|MinusPI), e1, e2, _) when isPointer e1 ->
      (* We've disallowed ptr arith if e1 is null. *)
      true
  | AddrOf lv
  | StartOf lv -> true
  | _ -> false

let isFalse state e =
  match e with
    UnOp(LNot, e', _) -> isNonNull state e'
  | _ -> isZero e


(* Update a state to reflect a branch *)
let doBranch (a:absState) (e:exp) : absState = 
(*   log "Guard %a.\n" dx_exp e; *)
  let e = match stripNopCasts e with
      UnOp(LNot, UnOp(LNot, e, _), _) -> e
    | e -> e
  in
  match e with
    Lval(Var vi, NoOffset) when isPointerType vi.vtype -> 
      addNonNull a vi
  | _ -> 
      a

(* Update a state to reflect a check *)
let processCheck a (c:check) : absState =
  match c with
    CNonNull e -> doBranch a e 
  | _ -> a

module Flow = struct
  let name = "DeputyOpt"
  let debug = ref false
  type t = absState
  let copy x = x
  let stmtStartData = stateMap
  let pretty = d_state
  let computeFirstPredecessor s a = a

  let combinePredecessors s ~(old:t) newa = 
    let nnv = List.filter 
                (fun vi -> List.memq vi newa.nonNullVars) 
                old.nonNullVars
    in
    if List.length nnv <> List.length old.nonNullVars then
      Some {old with nonNullVars = nnv}
    else
      None (* at fixed point *)

  let doInstr i a = 
(*     log "Visiting %a  State is %a.\n" dn_instr i d_state a; *)
    let a = match i with
        Set((Var vi, NoOffset), e, _) when isPointerType vi.vtype -> 
          if isNonNull a e then
            addNonNull a vi
          else
            scrambleVar a vi
      | Call(Some (Var vi, NoOffset), _, _, _) when isPointerType vi.vtype ->
          scrambleVar a vi
      | _ -> a
    in
    DF.Done a

  let doStmt s a = 
    curStmt := s.sid;
    (* Look for any checks associated with this stmt, and
       include that info in our state. *)
    let checks = GA.getg allChecks s.sid in
    let a = List.fold_left processCheck a checks in
    DF.SUse a

  let doGuard e a = 
    if isFalse a e then DF.GUnreachable
    else DF.GUse (doBranch a e)

  let filterStmt s = true
end

module FlowEngine = DF.ForwardsDataFlow (Flow)

let flowOptimizeCheck (c: check) ((inState, acc):(absState * check list))
  : (absState * check list) =
  let isNonNull = isNonNull inState in
  let state = processCheck inState c in
  match c with
  | CNonNull (e1) when isNonNull e1 ->
      state, acc
  | CNullOrLE (e1, e2, e3, why)
  | CNullOrLENT (e1, e2, e3, why) when isZero e1 ->
      state, acc
  | CNullOrLE (e1, e2, e3, why) when isNonNull e1 ->
      state, CUnsignedLE(e2, e3, why)::acc
  | CNullOrLENT (e1, e2, e3, why) when isNonNull e1 ->
      state, CUnsignedLENT(e2, e3, why)::acc
  | _ -> state, c::acc

let flowOptimizeVisitor = object (self)
  inherit nopCilVisitor

  method vstmt s =
    begin
      try
        let state = IH.find stateMap s.sid in
        let checks = GA.getg allChecks s.sid in
        (* Process the checks.  Use fold_right to start from the end of the
           list, because the list is in reverse order. *)
        let _, checks' = List.fold_right flowOptimizeCheck checks (state, []) in
        GA.set allChecks s.sid checks';
        (* Optimize branches *)
        begin
          match s.skind with
          | If(e, blk1, blk2, l) when isNonNull state e -> 
              s.skind <- Block blk1
          | If(e, blk1, blk2, l) when isFalse state e -> 
              s.skind <- Block blk2
          | _ -> ()
        end
      with Not_found -> () (* stmt is unreachable *)
    end;
    DoChildren

  method vfunc fd =
    curFunc := fd;
    let cleanup x = 
      curFunc := dummyFunDec; 
      x
    in
    ChangeDoChildrenPost (fd, cleanup)

end

let doFlowAnalysis (fd:fundec): unit =
  IH.clear stateMap;
  let fst = List.hd fd.sbody.bstmts in
  IH.add stateMap fst.sid top;
  FlowEngine.compute [fst];
  ignore (visitCilFunction flowOptimizeVisitor fd);
  IH.clear stateMap;
  curStmt := -1; 
  ()

(**************************************************************************)
(**************************************************************************)

let postPassVisitor1 = object (self)
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

  method vfunc fd =
    if isTrustedAttr fd.svar.vattr then
      SkipChildren
    else
      DoChildren
end

let postPassVisitor2 = object (self)
  inherit nopCilVisitor

  (* Remove any "bounds" or "fancybounds" annotations. *)
  method vattr a =
    match a with
    | Attr(("bounds" | "fancybounds" | "nullterm" | "trusted"
            | "when" | "fancywhen"), _) ->
        ChangeTo []
    | _ -> DoChildren

end

(**************************************************************************)

let checkFile (f: file) : unit =
  if !verbose then E.log "Using optimization level %d.\n" !optLevel;
  List.iter
    (fun global ->
       match global with
       | GVar (vi, _, _) ->
           assert vi.vglob;
           if vi.vstorage = Static then
             staticGlobalVars := vi :: !staticGlobalVars
           else
             nonStaticGlobalVars := vi :: !nonStaticGlobalVars
       | _ -> ())
    f.globals;
  visitCilFileSameGlobals preProcessVisitor f;
  visitCilFileSameGlobals polyVisitor f;
  visitCilFileSameGlobals inferVisitor f;
  if !inferFile <> "" then begin
    try
      let inferChannel = open_out !inferFile in
      dumpFile deputyPrinter inferChannel !inferFile f;
      close_out inferChannel
    with Sys_error _ ->
      E.s (E.error "Error dumping inference results to %s\n" !inferFile)
  end;
  allowChecks := true;
  List.iter
    (fun global ->
       currentLoc := get_globalLoc global;
       match global with
       | GType (ti, _) -> checkTypedef ti
       | GCompTag (ci, _) when ci.cstruct -> checkStruct ci
       | GVar (vi, init, _) -> checkVar vi init
       | GFun (fd, loc) ->
           if not (isTrustedAttr fd.svar.vattr) then begin
             checkFundec fd loc;
             if !optLevel = 1 then 
               ignore (visitCilFunction optimizeVisitor fd)
             else if !optLevel = 2 then begin
               ignore (visitCilFunction optimizeVisitor fd);
	       let cf = constFoldVisitor false in
               forwardTmpSub fd;
	       constProp fd;
	       ignore(visitCilFunction cf fd);
               doFlowAnalysis fd;
               forwardTmpSub fd;
	       constProp fd;
	       ignore(visitCilFunction cf fd);
	       ignore(visitCilFunction optimizeVisitor fd);
             end
          end
       | _ -> ())
    f.globals;
  (* Turn the check datastructure into explicit checks, so that they show up
     in the output. *)
  visitCilFileSameGlobals postPassVisitor1 f;
  visitCilFileSameGlobals postPassVisitor2 f;
  if !optLevel >= 2 then begin
    Cfg.clearFileCFG f; 
    Cfg.computeFileCFG f;
    Deadcodeelim.dce f;
    List.iter (fun g -> match g with GFun(fd,loc) ->
      forwardTmpSub fd | _ -> ()) f.globals;
    Deadcodeelim.dce f;
    List.iter (fun g -> match g with GFun(fd,loc) ->
      RCT.rm_unused_locals fd | _ -> ()) f.globals;
  end;
  f.globals <- (GText "#include <deputy/checks.h>\n\n")::f.globals;
  (* Tell CIL to put comments around the bounds attributes. *)
  print_CIL_Input := false;
  ()

let feature : featureDescr = 
  { fd_name = "Deputy";
    fd_enabled = ref false;
    fd_description = "Typecheck and instrument the program using Deputy.";
    fd_extraopt = [
    "--deputyverbose", Arg.Set verbose,
         "Enable verbose output for Deputy";
    "--deputyinferout", Arg.Set_string inferFile,
          "File in which to place Deputy inference results";
    "--deputyopt", Arg.Set_int optLevel,
         ("Control deputy optimizations:\n\t\t" ^
          "0: no optimization\n\t\t" ^
          "1: flow-insensitive optimization  (Default)\n\t\t" ^
          "2: all optimization");
    "--deputytrust", Arg.Set trustAll,
          "Trust all bad casts by default";
    ];
    fd_doit = checkFile;
    fd_post_check = true;
  } 
