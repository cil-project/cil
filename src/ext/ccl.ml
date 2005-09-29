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

let debug : bool ref = ref false

let globals : global list ref = ref []

let curLocation : location ref = ref locUnknown
let curFunction : fundec ref = ref dummyFunDec

type annot =
| AIgn
| AZero
| ANonZero
| AOne
| ASafe
| ANT of int
| ANTI of string * int
| ACC of int
| ACCB of string
| ACCBI of string
| AVC of string
| AVCB of string
| AVCBI of string

type fact = string * annot

module OrderedFact = struct
  type t = fact
  let compare = compare
end
module FactSet = Set.Make(OrderedFact)

type state = {
  mutable facts : FactSet.t;
}

type summary =
| SNone
| SInt of int
| SVar of string
| SVarOff of string * string
| SVarOffConst of string * int
| SDerefVar of string
| SDerefVarOff of string * string
| SDerefVarOffConst of string * int
| SAddrVar of string
| SFacts of FactSet.t

let d_annot () (annot : annot) : doc =
  match annot with
  | AIgn -> text "AIgn"
  | AZero -> text "AZero"
  | ANonZero -> text "ANonZero"
  | AOne -> text "AOne"
  | ASafe -> text "ASafe"
  | ANT n -> dprintf "ANT %d" n
  | ANTI (s, n) -> dprintf "ANTI %s %d" s n
  | ACC n -> dprintf "ACC %d" n
  | ACCB s -> dprintf "ACCB %s" s
  | ACCBI s -> dprintf "ACCBI %s" s
  | AVC s -> dprintf "AVC %s" s
  | AVCB s -> dprintf "AVCB %s" s
  | AVCBI s -> dprintf "AVCBI %s" s

let d_annots () (annots : annot list) : doc =
  seq (text ", ") (d_annot ()) annots

let d_fact () ((s, a) : fact) : doc =
  dprintf "  %s : %a\n" s d_annot a

let d_facts () (facts : FactSet.t) : doc =
  seq (text "") (d_fact ()) (FactSet.elements facts)

let d_state () (state : state) : doc =
  d_facts () state.facts

let d_summary () (sum : summary) : doc =
  match sum with
  | SNone -> dprintf "SNone"
  | SInt i -> dprintf "SInt %d" i
  | SVar s -> dprintf "SVar %s" s
  | SVarOff (s1, s2) -> dprintf "SVarOff %s %s" s1 s2
  | SVarOffConst (s, i) -> dprintf "SVarOffConst %s %d" s i
  | SDerefVar s -> dprintf "SDerefVar %s" s
  | SDerefVarOff (s1, s2) -> dprintf "SDerefVarOff %s %s" s1 s2
  | SDerefVarOffConst (s, i) -> dprintf "SDerefVarOffConst %s %d" s i
  | SAddrVar s -> dprintf "SAddrVar %s" s
  | SFacts _ -> dprintf "SFacts"

let isOutType (t : typ) : bool =
  hasAttribute "out" (typeAttrs t)

let varNameIsFS (name : string) : bool =
  try
    let vi = List.find (fun vi -> vi.vname = name) !curFunction.slocals in
    not vi.vaddrof
  with Not_found ->
    false

let varNameToInfo (name : string) : varinfo option =
  try
    Some (List.find
            (fun vi -> vi.vname = name)
            (!curFunction.slocals @ !curFunction.sformals))
  with Not_found ->
    None

let isLocalVar (name : string) : bool =
  match varNameToInfo name with
  | Some _ -> true
  | None -> false

let globalVarType (name : string) : typ =
  let t =
    List.fold_right
      (fun glob result ->
         match glob, result with
         | GVarDecl (vi, _), None
         | GVar (vi, _, _), None when vi.vname = name -> Some vi.vtype
         | _ -> result)
      !globals
      None
  in
  match t with
  | Some t' -> t'
  | None -> E.s (E.bug "couldn't find global %s\n" name)

let replaceName (name1 : string) (name2 : string)
                (facts : FactSet.t) : FactSet.t =
  FactSet.fold
    (fun (aname1, annot1) rest ->
       let aname2 = if aname1 = name1 then name2 else aname1 in
       let annot2 =
         match annot1 with
         | ANTI (vname1, n) when vname1 = name1 -> ANTI (name2, n)
         | AVC vname1 when vname1 = name1 -> AVC name2
         | AVCB vname1 when vname1 = name1 -> AVCB name2
         | AVCBI vname1 when vname1 = name1 -> AVCBI name2
         | ACCB vname1 when vname1 = name1 -> ACCB name2
         | ACCBI vname1 when vname1 = name1 -> ACCBI name2
         | ANTI _
         | AVC _
         | AVCB _
         | AVCBI _
         | ACCB _
         | ACCBI _
         | AIgn
         | AZero
         | ANonZero
         | AOne
         | ASafe
         | ANT _
         | ACC _ -> annot1
       in
       FactSet.add (aname2, annot2) rest)
    facts
    FactSet.empty

let getMaxFact (fn : fact -> int) (facts : FactSet.t) : int =
  FactSet.fold
    (fun fact cur -> max (fn fact) cur)
    facts
    (-1)

let getMaxACC (name : string) (facts : FactSet.t) : int =
  getMaxFact
    (fun fact ->
       match fact with
       | name', ACC n when name = name' -> n
       | _ -> -1)
    facts

let getMaxANT (name : string) (facts : FactSet.t) : int =
  getMaxFact
    (fun fact ->
       match fact with
       | name', ANT n when name = name' -> n
       | _ -> -1)
    facts

let getMaxANTI (name1 : string) (name2 : string) (facts : FactSet.t) : int =
  getMaxFact
    (fun fact ->
       match fact with
       | name1', ANTI (name2', n) when name1 = name1' && name2 = name2' -> n
       | _ -> -1)
    facts

let trimFacts (facts : FactSet.t) : FactSet.t =
  FactSet.fold
    (fun fact rest ->
       match fact with
       | name, ACC n when n < getMaxACC name facts -> rest
       | name, ANT n when n < getMaxANT name facts -> rest
       | name1, ANTI (name2, n) when n < getMaxANTI name1 name2 facts -> rest
       | _ -> FactSet.add fact rest)
    facts
    FactSet.empty

let joinFacts (facts1 : FactSet.t) (facts2 : FactSet.t) : FactSet.t =
  let facts1' = trimFacts facts1 in
  let facts2' = trimFacts facts2 in
  let join = FactSet.inter facts1' facts2' in
  FactSet.fold
    (fun fact rest ->
       let add fact' =
         FactSet.add fact' rest
       in
       match fact with
       | name, ACC n ->
           let m = getMaxACC name facts2' in
           if m >= 0 then
             add (name, ACC (min n m))
           else
             rest
       | name, ANT n ->
           let m = getMaxANT name facts2' in
           if m >= 0 then
             add (name, ANT (min n m))
           else
             rest
       | name1, ANTI (name2, n) ->
           let m = getMaxANTI name1 name2 facts2' in
           if m >= 0 then
             add (name1, ANTI (name2, min n m))
           else
             rest
       | _ -> rest)
    facts1'
    join

let closeFacts (facts : FactSet.t) : FactSet.t =
  (* Warning: This code may need to change for more complex closure rules. *)
  let closeAnnot (annot : annot) : annot list =
    annot ::
    match annot with
    | ANT _ -> [ ASafe ]
    | AZero -> [ ASafe; ANT 0 ]
    | AOne -> [ ANonZero ]
    | ACCB s -> [ ACCBI s ]
    | AVCB s -> [ AVCBI s ]
    | _ -> []
  in
  FactSet.fold
    (fun (name, annot) rest ->
       List.fold_right
         (fun annot' rest' -> FactSet.add (name, annot') rest')
         (closeAnnot annot)
         rest)
    facts
    FactSet.empty

let attrToFact (name : string) (attr : attribute) : fact option =
  match attr with
  | Attr ("ignore", []) -> Some (name, AIgn)
  | Attr ("nullterm", []) -> Some (name, ANT 0)
  | Attr ("safe", []) -> Some (name, ASafe)
  | Attr ("constcount", [AInt n]) -> Some (name, ACC n)
  | Attr ("varcount", [ACons (s, [])]) -> Some (name, AVC s)
  | Attr ("varcountof", [ACons (s, [])]) -> Some (s, AVC name)
  | _ -> None

let myAttr (attr : attribute) : bool =
  match attrToFact "*" attr with
  | Some _ -> true
  | None when attr = Attr ("out", []) -> true
  | None -> false

let attrsToFacts (name : string) (attrs : attributes) : FactSet.t =
  List.fold_right
    (fun attr rest ->
       match attrToFact name attr with
       | Some fact -> FactSet.add fact rest
       | None -> rest)
    attrs
    FactSet.empty

let typeToFacts (name : string) (t : typ) : FactSet.t =
  match unrollType t with
  | TArray (_, len, attrs) ->
      (* TODO: eliminate makeState code duplication *)
      begin
        try
          FactSet.add
            (name, ACC (lenOfArray len))
            (attrsToFacts name attrs)
        with LenOfArray ->
          attrsToFacts name attrs
      end
  | _ -> attrsToFacts name (typeAttrs t)

let makeState (fd : fundec) : state =
  let annots = Hashtbl.create 113 in
  let formalFacts =
    List.fold_right
      (fun vi rest -> FactSet.union (typeToFacts vi.vname vi.vtype) rest)
      fd.sformals
      FactSet.empty
  in
  let localFacts =
    List.fold_right
      (fun vi rest ->
         match unrollType vi.vtype with
         | TArray (_, len, _) ->
             begin
               try
                 FactSet.add (vi.vname, ACC (lenOfArray len))
                   (FactSet.union (attrsToFacts vi.vname vi.vattr) rest)
               with LenOfArray ->
                 FactSet.union (attrsToFacts vi.vname vi.vattr) rest
             end
         | _ ->
             if vi.vaddrof then
               FactSet.union (typeToFacts vi.vname vi.vtype) rest
             else
               rest)
      fd.slocals
      FactSet.empty
  in
  { facts = FactSet.union formalFacts localFacts; }

let copyState (s : state) : state =
  { facts = s.facts; }

let joinStates (s1 : state) (s2 : state) : state =
  { facts = joinFacts (closeFacts s1.facts) (closeFacts s2.facts); }

let equalFacts (f1 : FactSet.t) (f2 : FactSet.t) : bool =
  FactSet.equal (closeFacts f1) (closeFacts f2)

let equalStates (s1 : state) (s2 : state) : bool =
  equalFacts s1.facts s2.facts

let checkCast (toFacts : FactSet.t) (fromFacts : FactSet.t) : bool =
  let toClose = closeFacts toFacts in
  let fromClose = closeFacts fromFacts in
  let join = joinFacts toClose fromClose in
  FactSet.subset toClose join

let equalTypes (t1 : typ) (t2 : typ) : bool =
  let typeSigNC (t : typ) : typsig =
    typeSigWithAttrs (List.filter (fun attr -> attr <> Attr ("const", []))) t
  in
  (typeSigNC t1) = (typeSigNC t2)

let equalBaseTypes (t1 : typ) (t2 : typ) : bool =
  equalTypes (setTypeAttrs t1 []) (setTypeAttrs t2 [])

let equalTypesNoAttrs (t1 : typ) (t2 : typ) : bool =
  let typeSigNA (t : typ) : typsig =
    typeSigWithAttrs (List.filter (fun attr -> not (myAttr attr))) t
  in
  (typeSigNA t1) = (typeSigNA t2)

let dropCast (t : typ) (e : exp) : exp =
  match e with
  | CastE (t', e') when equalTypesNoAttrs t t' -> e'
  | _ -> e

class normVisitor = object
  inherit nopCilVisitor

  val mapping : (string, string) Hashtbl.t ref = ref (Hashtbl.create 1)

  method vtype (t : typ) : typ visitAction =
    match t with
    | TFun (rtype, args, vararg, attrs) ->
        let oldMapping = !mapping in
        let newMapping = Hashtbl.create 7 in
        let rec iter index args =
          match args with
          | (name, _, _) :: rest ->
              Hashtbl.add newMapping name (string_of_int index);
              iter (index + 1) rest
          | [] -> ()
        in
        iter 1 (argsToList args);
        Hashtbl.add newMapping "return" "0";
        mapping := newMapping;
        ChangeDoChildrenPost (t, (fun x -> mapping := oldMapping; x))
    | _ ->
        DoChildren

  method vattr (attr : attribute) : attribute list visitAction =
    match attr with
    | Attr ("varcount", [ACons (s, [])]) ->
        begin
          try
            let newAttr =
              Attr ("varcount", [ACons (Hashtbl.find !mapping s, [])])
            in
            ChangeTo [ newAttr ]
          with Not_found ->
            E.s (E.bug "error normalizing type\n")
        end
    | _ ->
        DoChildren
end

let normalizeType (t : typ) : typ =
  visitCilType (new normVisitor) t

class normVisitor2 subst = object
  inherit nopCilVisitor

  val subst = subst

  method vtype (t : typ) : typ visitAction =
    match t with
    | TFun _ -> SkipChildren
    | _ -> DoChildren

  method vattr (attr : attribute) : attribute list visitAction =
    match attr with
    | Attr (aname, [ACons (s, [])])
          when aname = "varcount" || aname = "varcountof" ->
        begin
          try
            let newAttr =
              match Hashtbl.find subst s with
              | SVar s' ->
                  [ Attr (aname, [ACons (s', [])]) ]
              | SInt i when aname = "varcount" ->
                  [ Attr ("constcount", [AInt i]) ]
              | SNone ->
                  []
              | _ ->
                  E.s (E.bug "unexpected summary\n")
            in
            ChangeTo newAttr
          with Not_found ->
            E.s (E.bug "%a: no substitution found for %s\n"
                       d_loc !curLocation s)
        end
    | _ ->
        DoChildren
end

let normalizeType2 (subst : (string, summary) Hashtbl.t) (t : typ) : typ =
  visitCilType (new normVisitor2 subst) t

let checkBaseTypes (toType : typ) (fromType : typ) : bool =
  let rec check (t1 : typ) (t2 : typ) (dontCheck : bool) : bool =
    (*ignore (E.log "checking %a = %a\n" d_type t1 d_type t2);*)
    match unrollType t1, unrollType t2 with
    | TPtr (t1', _), TPtr (t2', _) ->
        let f1 = typeToFacts "*" t1 in
        let f2 = typeToFacts "*" t2 in
        (dontCheck || equalFacts f1 f2) && check t1' t2' false
    | TFun _, TFun _ -> equalTypes (normalizeType t1) (normalizeType t2)
    | (TInt _ | TFloat _), (TInt _ | TFloat _) -> true
    | TInt _, TEnum _ -> true
    | TEnum _, TInt _ -> true
    | TInt _, TPtr _ -> true
    | TPtr _, TInt _ -> true (* TODO: improve this check *)
    | _, TVoid _ -> true (* TODO: improve this check *)
    | TVoid _, _ -> true (* TODO: improve this check *)
    | _, _ -> equalTypes t1 t2
  in
  let res = check toType fromType true in
  (*ignore (E.log "result: %b\n" res);*)
  res

let changeFacts (fn : fact -> fact list) (facts : FactSet.t) : FactSet.t =
  FactSet.fold
    (fun fact rest ->
       List.fold_right
         (fun fact' rest' -> FactSet.add fact' rest')
         (fn fact) rest)
    (closeFacts facts)
    FactSet.empty

let changeState (fn : fact -> fact list) (state : state) : unit =
  state.facts <- changeFacts fn state.facts

let changeAnnots (fn : annot -> annot list) (facts : FactSet.t) : FactSet.t =
  FactSet.fold
    (fun (name, annot) rest ->
       List.fold_right
         (fun annot' rest' -> FactSet.add (name, annot') rest')
         (fn annot) rest)
    (closeFacts facts)
    FactSet.empty

let summaryToFacts (sum : summary) (state : state) : FactSet.t =
  match sum with
  | SNone ->
      FactSet.empty
  | SInt i ->
      let annots =
        (* TODO: refacor the following *)
        if i = 0 then
          FactSet.fold
            (fun fact rest ->
               match fact with
               | name, ANT n -> (ANTI (name, n)) :: rest
               | name, ACC n when n > i -> (ACCB name) :: rest
               | name, AVC _ -> (AVCBI name) :: rest
               | _ -> rest)
            state.facts
            [ AZero ]
        else if i = 1 then
          FactSet.fold
            (fun fact rest ->
               match fact with
               | name, ANT n when n >= i -> (ANTI (name, n - i)) :: rest
               | name, ACC n when n > i -> (ACCB name) :: rest
               | name, AVC _ -> (AVCBI name) :: rest
               | _ -> rest)
            state.facts
            [ AOne ]
        else
          FactSet.fold
            (fun fact rest ->
               match fact with
               | name, ACC n when n > i && i > 0 -> (ACCB name) :: rest
               | name, AVC _ when i > 0 -> (AVCBI name) :: rest
               | _ -> rest)
            state.facts
            [ ANonZero ]
      in
      let extra =
          FactSet.fold
            (fun fact rest ->
               match fact with
               | name, ACC n when 0 <= i && i <= n ->
                   FactSet.add (name, (AVC "*")) rest
               | _ -> rest)
            state.facts
            FactSet.empty
      in
      List.fold_right
        (fun annot rest -> FactSet.add ("*", annot) rest)
        annots extra
  | SVar vname ->
      if isLocalVar vname then
        FactSet.fold
          (fun (name, annot) rest ->
             if name = vname then
               FactSet.add ("*", annot) rest
             else begin
               match annot with
               | AVC sname when sname = vname ->
                   FactSet.add (name, AVC "*") rest
               | _ -> rest
             end)
          state.facts
          FactSet.empty
      else
        typeToFacts "*" (globalVarType vname)
  | SVarOff (vname, oname) ->
      FactSet.fold
        (fun fact rest ->
           match fact with
           | vname', ANT _ when vname = vname' ->
               let maxAnti = getMaxANTI oname vname state.facts in
               if maxAnti >= 0 then
                 FactSet.add ("*", ANT maxAnti) rest
               else
                 rest
           | vname', ACC _ when vname = vname' ->
               if FactSet.mem (oname, ACCB vname) state.facts then
                 FactSet.add ("*", ASafe) rest
               else
                 rest
           | vname', AVC _ when vname = vname' ->
               if FactSet.mem (oname, AVCB vname) state.facts then
                 FactSet.add ("*", ASafe) rest
               else
                 rest
           | _ ->
               rest)
        state.facts
        FactSet.empty
  | SVarOffConst (vname, off) ->
      if off = 1 then begin
        changeFacts
          (fun (vname', annot) ->
             if vname = vname' then
               match annot with
               | ANT n when n >= off -> [ ("*", ANT (n - off)) ]
               | ANTI (s, n) when n >= off -> [ ("*", ANTI (s, n - off)) ]
               | ACCB s -> [ ("*", ACCBI s) ]
               | AVCB s -> [ ("*", AVCBI s) ]
               | AZero  -> [ ("*", AOne) ]
               | _ -> []
             else
               [])
          state.facts
      end else
        FactSet.empty
  | SDerefVar vname
  | SDerefVarOff (vname, _)
  | SDerefVarOffConst (vname, _) ->
      begin
        match varNameToInfo vname with
        | Some vi ->
            typeToFacts "*"
              (typeOfLval (Mem (Lval (Var vi, NoOffset)), NoOffset))
        | None -> FactSet.empty
      end
  | SAddrVar vname ->
      FactSet.singleton ("*", ASafe)
  | SFacts facts ->
      facts

let safeDeref (facts : FactSet.t) : bool =
  FactSet.exists (fun fact -> fact = ("*", ASafe)) (closeFacts facts)

let hasAnnot (a : annot) (facts : FactSet.t) : bool =
  FactSet.mem ("*", a) (closeFacts facts)

let summaryIsZero (sum : summary) (state : state) : bool =
  hasAnnot AZero (summaryToFacts sum state)

let summaryIsNonZero (sum : summary) (state : state) : bool =
  hasAnnot ANonZero (summaryToFacts sum state)

let rec evaluateExp (e : exp) (state : state) : summary =
  match e with
  | UnOp (op, e', _) -> SNone
  | BinOp ((PlusA | PlusPI | IndexPI), e1, e2, _) ->
      let s1 = evaluateExp e1 state in
      let s2 = evaluateExp e2 state in
      begin
        match s1, s2 with
        | SVar v1, SVar v2 ->
            SVarOff (v1, v2)
        | SVar v1, _ ->
            let f2 = summaryToFacts s2 state in
            if hasAnnot AZero f2 then
              SVar v1
            else if hasAnnot AOne f2 then
              SVarOffConst (v1, 1)
            else
              SNone
        | _, _ ->
            let f1 = summaryToFacts s1 state in
            let f2 = summaryToFacts s2 state in
            if hasAnnot AOne f2 then begin
              let facts =
                changeAnnots
                  (fun annot ->
                     match annot with
                     | ANT 2 -> [ ANT 1 ]
                     | ANT 1 -> [ ANT 0 ]
                     | ANTI (s, 1) -> [ (ANTI (s, 0)) ]
                     | ACCB s -> [ (ACCBI s) ]
                     | AVCB s -> [ (AVCBI s) ]
                     | AZero  -> [ AOne ]
                     | _ -> [])
                  f1
              in
              SFacts facts
            end else if hasAnnot AZero f2 then begin
              s1
            end else begin
              SNone
            end
      end
  | BinOp (op, e1, e2, _) -> SNone
  | AddrOf lv ->
      begin
        match evaluateLval lv state with
        | SVar vname -> SAddrVar vname
        | _ -> SFacts (FactSet.singleton ("*", ASafe))
      end
  | Lval lv -> evaluateLval lv state
  | CastE (t, e') ->
      let eSum = evaluateExp e' state in
      let eFacts = summaryToFacts eSum state in
      let eType = typeOf e' in
      let tFacts = typeToFacts "*" t in
      (* TODO: NULL is defined as ((void* )0), so we hack around it... *)
      if hasAnnot AZero eFacts then
        eSum
      (* TODO: character comparisons get cast to ints, but we need to
         pass the summary through in order to recognize the conditional *)
      else if isIntegralType eType && isIntegralType t then
        eSum
      (* TODO: CIL inserts casts where toplevel annots don't match *)
      else if equalBaseTypes eType t then
        eSum
      else begin
        if not (hasAnnot AIgn tFacts) then begin
          if not (checkBaseTypes t eType) then
            E.s (E.bug "%a: bad cast: %a <- %a\n" d_loc !curLocation
                       d_type t d_type eType);
          if not (checkCast tFacts eFacts) then
            E.s (E.bug "%a: bad cast: %a <- %a\n" d_loc !curLocation
                       d_facts tFacts d_facts eFacts)
        end;
        SFacts tFacts
      end
  | Const (CStr s) ->
      SFacts (FactSet.singleton ("*", ANT 0))
  | Const _ ->
      begin
        match isInteger e with
        | Some i -> SInt (Int64.to_int i) (* TODO: possible bug in conv?  *)
        | None -> SNone
      end
  | SizeOf _
  | SizeOfE _
  | SizeOfStr _ ->
      let e' = constFold true e in
      begin
        match e' with
        | Const _ -> ()
        | _ -> E.s (E.bug "expected constant\n")
      end;
      evaluateExp e' state
  | AlignOf _
  | AlignOfE _ -> SNone
  | StartOf lv -> evaluateLval lv state

and evaluateLval (lv : lval) (state : state) : summary =
  match lv with
  | Var vi, NoOffset ->
      SVar vi.vname
  | Var _, _ ->
      SFacts (typeToFacts "*" (typeOfLval lv))
  | Mem e, off ->
      let s = evaluateExp e state in
      if not (safeDeref (summaryToFacts s state)) then
        E.s (E.bug "%a: not safe to deref %a\n" d_loc !curLocation d_exp e);
      begin
        match s, off with
        | SVar name, NoOffset -> SDerefVar name
        | SVarOff (bname, oname), NoOffset -> SDerefVarOff (bname, oname)
        | SVarOffConst (name, off), NoOffset -> SDerefVarOffConst (name, off)
        | _ -> SFacts (typeToFacts "*" (typeOfLval lv))
      end

let analyzeCond (cond : exp) (state : state) : unit =
  let upgradeANT (n : int) (vname : string) : unit =
    changeState
      (fun (name, annot) ->
         match annot with
         | ANT m when name = vname && n = m ->
             [ (name, ANT n); (name, ANT (n + 1)) ]
         | _ -> [ (name, annot) ])
      state
  in
  let upgradeANTI (n : int) (vname : string) (sname : string) : unit =
    changeState
      (fun (name, annot) ->
         match annot with
         | ANTI (name', m) when name = vname && name' = sname && n = m ->
             [ (name, ANTI (name', n + 1)) ]
         | _ -> [ (name, annot) ])
      state
  in
  let upgradeACCBI (vname : string) (aname : string) : unit =
    changeState
      (fun (name, annot) ->
         match annot with
         | ACCBI name' when name = vname && name' = aname ->
             [ (name, ACCB name') ]
         | _ -> [ (name, annot) ])
      state
  in
  let upgradeAVCBI (vname : string) (aname : string) : unit =
    changeState
      (fun (name, annot) ->
         match annot with
         | AVCBI name' when name = vname && name' = aname ->
             [ (name, AVCB name') ]
         | _ -> [ (name, annot) ])
      state
  in
  let equalNonZero (e : exp) (sum : summary) : unit =
    match sum with
    | SDerefVar vname ->
        upgradeANT 0 vname
    | SDerefVarOff (bname, oname)
          when FactSet.mem (oname, ANTI (bname, 0)) state.facts ->
        upgradeANTI 0 oname bname
    | SDerefVarOffConst (vname, 1) ->
        upgradeANT 1 vname
    | _ ->
        ignore (E.log "unrecognized zero exp: %a == 0\n" d_exp e)
  in
  let checkLessThan (e1 : exp) (e2 : exp) : unit =
    let s1 = evaluateExp e1 state in
    let s2 = evaluateExp e2 state in
    match s1 with
    | SVar vname ->
        begin
          match isInteger e2 with
          | Some i ->
              let arrays =
                FactSet.fold
                  (fun (name, annot) rest ->
                     if annot = ACC (Int64.to_int i) then
                       name :: rest
                     else
                       rest)
                  state.facts
                  []
              in
              List.iter (fun aname -> upgradeACCBI vname aname) arrays
          | None ->
              begin
                match s2 with
                | SVar bname ->
                    let arrays =
                      FactSet.fold
                        (fun (name, annot) rest ->
                           if annot = AVC bname then
                             name :: rest
                           else
                             rest)
                        state.facts
                        []
                    in
                    List.iter (fun aname -> upgradeAVCBI vname aname) arrays
                | _ -> ()
              end
        end
    | _ -> ()
  in
  let checkEquality (e1 : exp) (e2 : exp) : unit =
    let s1 = evaluateExp e1 state in
    let s2 = evaluateExp e2 state in
    if summaryIsNonZero s2 state then
      equalNonZero e1 s1
  in
  let checkDisequality (e1 : exp) (e2 : exp) : unit =
    let s1 = evaluateExp e1 state in
    let s2 = evaluateExp e2 state in
    if summaryIsZero s2 state then
      equalNonZero e1 s1
  in
  let rec checkCond (cond : exp) (invert : bool) : unit =
    match cond with
    | UnOp (LNot, cond', _) ->
        checkCond cond' (not invert)
    | BinOp ((LAnd | LOr), _, _, _) ->
        E.s (E.bug "&& or || not eliminated by cil\n")
    | BinOp (Lt, cond1, cond2, _) ->
        checkLessThan cond1 cond2
    | BinOp (Eq, cond1, cond2, _) ->
        if invert then
          checkDisequality cond1 cond2
        else
          checkEquality cond1 cond2
    | BinOp (Ne, cond1, cond2, _) ->
        if invert then
          checkEquality cond1 cond2
        else
          checkDisequality cond1 cond2
    | Lval lv ->
        if invert then
          checkEquality cond zero
        else
          checkDisequality cond zero
    | _ ->
        ignore (E.log "unrecognized cond: %a\n" d_exp cond)
  in
  ignore (E.log "%a: cond %a\n%a\n" d_loc !curLocation
                d_exp cond d_state state);
  ignore (evaluateExp cond state);
  checkCond cond false

let analyzeStmt (stmt : stmt) (state : state) : unit =
  match stmt.skind with
  | Instr instrs ->
      List.iter
        (fun instr ->
           let doSetNames (vnames : string list) (facts : FactSet.t) : unit =
             let removed =
               FactSet.fold
                 (fun (name, annot) rest ->
                    match annot with
                    | ANTI (vname', _)
                    | AVC vname'
                    | AVCB vname'
                    | AVCBI vname'
                    | ACCB vname'
                    | ACCBI vname' when List.mem vname' vnames -> rest
                    | _ when List.mem name vnames -> rest
                    | _ -> FactSet.add (name, annot) rest)
                 state.facts
                 FactSet.empty
             in
             state.facts <- FactSet.union removed facts;
             (*
             ignore (E.log "%a: %s gets %a\n" d_loc !curLocation
                           vname d_facts facts)
             *)
           in
           let doSet (lv : lval) (eType : typ) (facts : FactSet.t) : unit =
             let lvType = typeOfLval lv in
             let lvSum = evaluateLval lv state in
             if not (checkBaseTypes lvType eType) then
               E.s (E.bug "%a: bad cast: %a <- %a\n" d_loc !curLocation
                          d_type lvType d_type eType);
             begin
               match lvSum with
               | SVar vname when varNameIsFS vname ->
                   doSetNames [ vname ] (replaceName "*" vname facts)
               | _ ->
                   (* check base types equal *)
                   let lvFacts = summaryToFacts lvSum state in
                   if not (checkCast lvFacts facts) then
                     E.s (E.bug "%a: bad cast: %a <- %a\n" d_loc !curLocation
                                d_facts lvFacts d_facts facts)
             end
           in
           ignore (E.log "%a: instr %a\n%a\n" d_loc !curLocation
                         d_instr instr d_state state);
           match instr with
           | Call (ret, fn, actuals, l) ->
               curLocation := l;
               begin
                 match typeOf fn with
                 | TFun (_, None, _, _) ->
                     E.s (E.bug "not enough function type info\n")
                 | TFun (rtype, Some argInfo, isVarArg, _) ->
                     let matches = Hashtbl.create 7 in
                     let removeNames = ref [] in
                     let addFacts = ref FactSet.empty in
                     let rec matchNames formals actuals : unit =
                       match formals, actuals with
                       | (fname, ftype, _) :: frest, aExp :: arest ->
                           if fname <> "" then begin
                             let aExp' = dropCast ftype aExp in
                             let aType = typeOf aExp' in
                             let aSum = evaluateExp aExp' state in
                             match isOutType ftype, isOutType aType, aSum with
                             | false, false, SInt i ->
                                 ignore (E.log "adding %s -> %d\n" fname i);
                                 Hashtbl.add matches fname aSum
                             | true, false, SInt 0 ->
                                 ignore (E.log "adding %s -> 0\n" fname);
                                 Hashtbl.add matches fname SNone
                             | false, false, SVar vname
                             | true, true, SVar vname
                             | true, false, SAddrVar vname ->
                                 ignore (E.log "adding %s -> %s\n" fname vname);
                                 Hashtbl.add matches fname (SVar vname)
                             | _ ->
                                 ()
                           end;
                           matchNames frest arest
                       | [], [] ->
                           ()
                       | [], _ :: _ ->
                           if isVarArg then
                             ignore (E.log "ignoring vararg args\n")
                           else
                             E.s (E.bug "too many actuals\n")
                       | _ :: _, [] ->
                           E.s (E.bug "too many formals\n")
                     in
                     let rec checkArgs formals actuals : unit =
                       match formals, actuals with
                       | (fname, ftype, _) :: frest, aExp :: arest ->
                           if not (isOutType ftype) then begin
                             let fFacts = normalizeType2 matches ftype in
                             let fFacts' = typeToFacts "*" fFacts in
                             let aExp' = dropCast ftype aExp in
                             let aSum = evaluateExp aExp' state in
                             if not (hasAnnot AIgn fFacts') then begin
                               if isPointerType ftype &&
                                  FactSet.is_empty fFacts' then begin
                                 let fname =
                                   match fn with
                                   | Lval (Var vi, NoOffset) -> vi.vname
                                   | _ -> "unknown"
                                 in
                                 ignore (E.log "%a: ptr formal has no annots (%s)\n"
                                               d_loc l fname)
                               end;
                               let fType = ftype in
                               let aType = typeOf aExp' in
                               let aFacts = summaryToFacts aSum state in
                               if not (checkBaseTypes fType aType) then
                                 E.s (E.bug "%a: bad arg: %a <- %a\n" d_loc l
                                            d_type fType d_type aType);
                               if not (checkCast fFacts' aFacts) then
                                 E.s (E.bug "%a: bad arg: %a <- %a\n" d_loc l
                                            d_facts fFacts' d_facts aFacts)
                             end
                           end;
                           checkArgs frest arest
                       | [], [] ->
                           ()
                       | [], _ :: _ ->
                           if isVarArg then
                             ignore (E.log "ignoring vararg args\n")
                           else
                             E.s (E.bug "too many actuals\n")
                       | _ :: _, [] ->
                           E.s (E.bug "too many formals\n")
                     in
                     (* TODO: verify out vars used properly *)
                     let rec checkReturns formals actuals : unit =
                       match formals, actuals with
                       | (fname, ftype, _) :: frest, aExp :: arest ->
                           if isOutType ftype then begin
                             let ftype' = normalizeType2 matches ftype in
                             let ftype'' =
                               match ftype' with
                               | TPtr (bt, _) -> bt
                               | _ -> E.s (E.bug "expected ptr type\n")
                             in
                             let fFacts' = typeToFacts "*" ftype'' in
                             let aExp' = dropCast ftype' aExp in
                             let aSum = evaluateExp aExp' state in
                             if not (hasAnnot AIgn fFacts') then begin
                               if isPointerType ftype'' &&
                                  FactSet.is_empty fFacts' then begin
                                 let fname =
                                   match fn with
                                   | Lval (Var vi, NoOffset) -> vi.vname
                                   | _ -> "unknown"
                                 in
                                 ignore (E.log "%a: ptr formal has no annots (%s)\n"
                                               d_loc l fname)
                               end;
                               let fType = ftype' in
                               let aType = typeOf aExp' in
                               begin
                                 match isOutType aType, aSum with
                                 | true, SVar vname ->
                                     () (* TODO: check type! *)
                                 | false, SAddrVar vname ->
                                     removeNames := vname :: !removeNames;
                                     addFacts :=
                                       FactSet.union
                                         (replaceName "*" vname fFacts')
                                         !addFacts;
                                 | false, SInt 0 -> ()
                                 | _ -> E.s (E.bug "expected addr of var\n")
                               end;
                               if not (checkBaseTypes aType fType) then
                                 E.s (E.bug "%a: bad out: %a <- %a\n" d_loc l
                                            d_type aType d_type fType);
                               (* TODO: Cast check covered above? *)
                               (*
                               if not (checkCast aFacts' fFacts) then
                                 E.s (E.bug "%a: bad out: %a <- %a\n" d_loc l
                                            d_facts aFacts' d_facts fFacts)
                               *)
                             end
                           end;
                           checkReturns frest arest
                       | [], [] ->
                           ()
                       | [], _ :: _ ->
                           if isVarArg then
                             ignore (E.log "ignoring vararg args\n")
                           else
                             E.s (E.bug "too many actuals\n")
                       | _ :: _, [] ->
                           E.s (E.bug "too many formals\n")
                     in
                     matchNames argInfo actuals;
                     checkArgs argInfo actuals;
                     checkReturns argInfo actuals;
                     doSetNames !removeNames !addFacts;
                     begin
                       match ret with
                       | Some lv ->
                           let facts =
                             typeToFacts "*" (normalizeType2 matches rtype)
                           in
                           doSet lv rtype facts
                       | None -> ()
                     end
                 | _ ->
                     E.s (E.bug "function has non-function type\n")
               end
           | Set (lv, e, l) ->
               curLocation := l;
               doSet lv (typeOf e) (summaryToFacts (evaluateExp e state) state)
           | Asm _ -> E.s (E.unimp "asm"))
        instrs
  | Return (eo, l) ->
      curLocation := l;
      begin
        match eo with
        | Some e ->
            let fType =
              match !curFunction.svar.vtype with
              | TFun (rtype, _, _, _) -> rtype
              | _ -> E.s (E.bug "expected function type\n")
            in
            let eType = typeOf e in
            if not (checkBaseTypes fType eType) then
              E.s (E.bug "%a: bad return: %a <- %a\n" d_loc l
                         d_type fType d_type eType);
            let fFacts = typeToFacts "*" fType in
            let eFacts = summaryToFacts (evaluateExp e state) state in
            if not (checkCast fFacts eFacts) then
              E.s (E.bug "%a: bad return: %a <- %a\n" d_loc l
                         d_facts fFacts d_facts eFacts)
        | None -> ()
      end
  | Loop _
  | Goto _
  | Block _ -> ()
  | If _ -> E.s (E.bug "if statement not handled separately")
  | Break _
  | Switch _
  | Continue _ -> E.s (E.bug "break, switch, or continue not removed")
  | TryFinally _
  | TryExcept _ -> E.s (E.unimp "exceptions")

let analyzeFundec (fd : fundec) : unit =
  curFunction := fd;
  let stmtState = Hashtbl.create 113 in
  let worklist = Stack.create () in
  let firstStmt = List.hd fd.sbody.bstmts in
  let firstState = makeState fd in
  try
  Hashtbl.add stmtState firstStmt.sid firstState;
  Stack.push firstStmt worklist;
  while not (Stack.is_empty worklist) do
    let stmt = Stack.pop worklist in
    let state =
      try
        Hashtbl.find stmtState stmt.sid
      with Not_found ->
        E.s (E.bug "analyzeAlloc: state not found\n");
    in
    let recordState (newState : state) (succ : stmt) : unit =
      try
        let succState = Hashtbl.find stmtState succ.sid in
        if not (equalStates newState succState) then begin
          curLocation := get_stmtLoc succ.skind;
          Hashtbl.replace stmtState succ.sid
                          (joinStates newState succState);
          Stack.push succ worklist;
        end
      with Not_found ->
        begin
          Hashtbl.replace stmtState succ.sid newState;
          Stack.push succ worklist;
        end
    in
    match stmt.skind with
    | If (cond, thenBranch, elseBranch, l) ->
        curLocation := l;
        let getBranchStmt (branch : block) : stmt =
          try
            List.hd branch.bstmts
          with Failure "hd" ->
            dummyStmt
        in
        let thenStmt = getBranchStmt thenBranch in
        let elseStmt = getBranchStmt elseBranch in
        let otherStmts =
          List.filter
            (fun succ -> succ.sid <> thenStmt.sid &&
                         succ.sid <> elseStmt.sid)
            stmt.succs
        in
        let handleStmt (cond : exp) (succ : stmt) : unit =
          let newState = copyState state in
          analyzeCond cond newState;
          recordState newState succ;
        in
        begin
          match otherStmts with
          | [] ->
              if thenStmt == dummyStmt || elseStmt == dummyStmt then
                E.s (E.bug "can't handle if statement succs\n");
              handleStmt cond thenStmt;
              handleStmt (UnOp (LNot, cond, intType)) elseStmt;
          | [otherStmt] ->
              if thenStmt != dummyStmt && elseStmt != dummyStmt then
                E.s (E.bug "can't handle if statement succs\n");
              handleStmt cond
                (if thenStmt == dummyStmt then otherStmt else thenStmt);
              handleStmt (UnOp (LNot, cond, intType))
                (if elseStmt == dummyStmt then otherStmt else elseStmt);
          | _ ->
              E.s (E.bug "can't handle if statement succs\n")
        end
    | _ ->
        begin
          let newState = copyState state in
          analyzeStmt stmt newState;
          if List.length stmt.succs > 0 then
            List.iter (recordState newState) stmt.succs
        end
  done
  with E.Error ->
    begin
    (*
    let worklist2 = Stack.create () in
    let donelist = Hashtbl.create 113 in
    Stack.push firstStmt worklist2;
    while not (Stack.is_empty worklist2) do
      let stmt = Stack.pop worklist2 in
      let state =
        try
          Hashtbl.find stmtState stmt.sid
        with Not_found ->
          { facts = FactSet.empty; }
      in
      ignore (E.log "%a: %a\n%a\n" d_loc (get_stmtLoc stmt.skind)
                    d_stmt stmt d_state state);
      Hashtbl.add donelist stmt.sid ();
      let sortedSuccs =
        List.sort
          (fun s2 s1 -> compare (get_stmtLoc s1.skind).line
                                (get_stmtLoc s2.skind).line)
          stmt.succs
      in
      List.iter
        (fun succ ->
           if not (Hashtbl.mem donelist succ.sid) then
             Stack.push succ worklist2)
        sortedSuccs
    done;
    *)
    raise E.Error
    end

class ptrArithVisitor = object
  inherit nopCilVisitor

  method vfunc (fd : fundec) =
    prepareCFG fd;
    computeCFGInfo fd false;
    analyzeFundec fd;
    DoChildren
end

(*
class dropAttrCastVisitor = object
  inherit nopCilVisitor

  method vinst (inst : instr) =
    begin
      match inst with
      | Call (_, fn, args, _) ->
          begin
            match typeOf fn with
            | TFun (_, Some argInfo, isVarArg, _) ->
            | _ -> E.s (E.bug "expected function type\n");
          end
      | _ -> ()
    end;
    DoChildren
end
*)

let analyzeFile (f : file) : unit =
  ignore (Partial.calls_end_basic_blocks f);
  ignore (Partial.globally_unique_vids f);
  globals := f.globals;
  visitCilFile (new ptrArithVisitor) f

let feature : featureDescr = 
  { fd_name = "Ptr";
    fd_enabled = ref false;
    fd_description = "find pointer arithmetic";
    fd_extraopt = [];
    fd_doit = analyzeFile;
    fd_post_check = true;
  } 
