(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 *  Sumit Gulwani       <gulwani@cs.berkeley.edu>
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

(* This module simplifies the expressions in a program in the following ways:
 
1. All expressions are either 

 basic::=
    Const _ 
    Addrof(Var v, NoOffset)
    StartOf(Var v, NoOffset)
    Lval(Var v, off), where v is a variable whose address is not taken
                      and off contains only "basic"

 exp::=
    basic
    Lval(Mem basic, NoOffset)
    BinOp(bop, basic, basic)
    UnOp(uop, basic)
    CastE(t, basic)
   
 lval ::= 
    Mem basic, NoOffset
    Var v, off, where v is a variable whose address is not taken and off
                contains only "basic"

 - all sizeof and alignof are turned into constants
 - accesses to variables whose address is taken is turned into "Mem" accesses
 - same for accesses to arrays
 - all field and index computations are turned into address arithmetic, 
   including bitfields.

*)


open Pretty
open Cil
open Feature
module E = Errormsg
module H = Hashtbl

type taExp = exp (* Three address expression *)
type bExp = exp  (* Basic expression *)

let debug = true

(* Whether to split structs *)
let splitStructs = ref true

(* Whether to simplify inside of Mem *)
let simpleMem = ref true
let simplAddrOf = ref true

(* Whether to convert function calls to calls-by-pointer when function address
 * has been taken somewhere. *)
let convertDirectCalls = ref true

(* Whether to convert field offsets to offsets by integer.
 * This conversion makes the generated code analysis simpler for static source
 * code verifiers. *)
let convertFieldOffsets = ref true
(* WARN: splitStructs should be set to false if field offsets are not
 * converted.  Otherwise, the connection between a pointer to a structure and
 * its fields is sometimes lost, and is harder to analyze statically.  If a
 * structure inside a structure (say, "struct A{struct B b} a;" is split into
 * fields, then, instead of a pointer to the enclosed structure (in "&a.b"), a
 * pointer to its first member might be used.  This will make the rest of the
 * structure pointed to by "&a.b" be accessed through the (possibly
 * non-structure) pointer to its first element, which is harder to analyze
 * statically.
 *
 * Last, but not least, this inconsistency will trigger "Cannot find
 * component .foo of bar" error if you turn splitting structure on and take an
 * address of  bar.foo, where foo is a field of a structure type. :-)
*)

let onlyVariableBasics = ref false
let noStringConstantsBasics = ref false

exception BitfieldAccess

(* Turn an expression into a three address expression (and queue some 
 * instructions in the process) *)
let rec makeThreeAddress 
    (setTemp: taExp -> bExp) (* Given an expression save it into a temp and 
                              * return that temp *)
    (e: exp) : taExp = 
  match e with 
    SizeOf _ | SizeOfE _ | AlignOf _ |  AlignOfE _ | SizeOfStr _ -> 
      constFold true e
  | Const _ -> e
  | AddrOf (Var _, NoOffset) -> e
  | AddrOfLabel (_) -> e
  | Lval lv -> Lval (simplifyLval setTemp lv)
  | BinOp(bo, e1, e2, tres) -> 
      BinOp(bo, makeBasic setTemp e1, makeBasic setTemp e2, tres)
  | Question _ ->
      E.s (bug "Simplify: There should not be a \"?:\" operator here.")
  | UnOp(uo, e1, tres) -> 
      UnOp(uo, makeBasic setTemp e1, tres)
  | CastE(t, e) -> 
      CastE(t, makeBasic setTemp e)
  | AddrOf lv -> begin
      if not(!simplAddrOf) then e else
      match simplifyLval setTemp lv with 
        Mem a, NoOffset -> if !simpleMem then a else AddrOf(Mem a, NoOffset)
      (* Do not change addrof if we do not convert field offsets *)
      | Mem a, off when not !convertFieldOffsets -> AddrOf(Mem a, off)
      (* Do not change addrof if we do not convert field offsets *)
      | Var v, off when not !convertFieldOffsets -> AddrOf(Var v, off)
      | _ -> (* This is impossible, because we are taking the address 
          * of v and simplifyLval should turn it into a Mem, except if the 
          * sizeof has failed.  *)
          E.s (bug "Simplify: makeThreeAddress for AddrOf(LV=%a, LVT=%a)"
              d_lval lv d_type (typeOfLval lv))
  end
  | StartOf lv -> 
      makeThreeAddress setTemp (AddrOf (addOffsetLval (Index(zero, NoOffset))
                                          lv))

(* Make a basic expression *)      
and makeBasic (setTemp: taExp -> bExp) (e: exp) : bExp = 
  let dump = false (* !currentLoc.line = 395 *) in
  if dump then
    ignore (E.log "makeBasic %a\n" d_plainexp e);
  (* Make it a three address expression first *)
  let e' = makeThreeAddress setTemp e in
  if dump then 
    ignore (E.log "   e'= %a\n" d_plainexp e');
  (* See if it is a basic one *)
  match e' with 
  | Lval (Var _, _) -> e'
  | Const _ | AddrOf (Var _, NoOffset) | StartOf (Var _, NoOffset) ->
      if !onlyVariableBasics then setTemp e' else e'
  | SizeOf _ | SizeOfE _ | AlignOf _ |  AlignOfE _ | SizeOfStr _ -> 
      E.s (bug "Simplify: makeBasic found SizeOf: %a" d_exp e')

   (* We cannot make a function to be Basic, unless it actually is a variable 
    * already. If this is a function pointer the best we can do is to make 
    * the address of the function basic *)
  | Lval (Mem a, NoOffset) when isFunctionType (typeOf e') -> 
      if dump then 
        ignore (E.log "  a function type\n");
      let a' = makeBasic setTemp a in
      Lval (Mem a', NoOffset)

  | AddrOf lv when not(!simplAddrOf) -> e'

  | _ -> begin
    if dump then ignore (E.log "Placing %a into a temporary\n" d_plainexp e');
    setTemp e' (* Put it into a temporary otherwise *)
  end


and simplifyLval 
    (setTemp: taExp -> bExp) 
    (lv: lval) : lval = 
  (* Add, watching for a zero *)
  let add (e1: exp) (e2: exp) = 
    if isZero e2 then e1 else BinOp(PlusA, e1, e2, !upointType) 
  in
  (* Convert an offset to an integer, and possibly a residual bitfield offset*)
  let rec offsetToInt 
      (t: typ) (* The type of the host *)
      (off: offset) : exp * offset = 
    match off with 
      NoOffset -> zero, NoOffset
    | Field(fi, off') -> begin
        let start = 
          try 
            let start, _ = bitsOffset t (Field(fi, NoOffset)) in
            start
          with SizeOfError (whystr, t') -> 
            E.s (E.bug "%a: Cannot compute sizeof: %s: %a"
                   d_loc !currentLoc whystr d_type t')
        in
        if start land 7 <> 0 then begin
          (* We have a bitfield *)
          assert (off' = NoOffset);
          zero, Field(fi, off')
        end else begin
          let next, restoff = offsetToInt fi.ftype off' in
          add (integer (start / 8)) next,  restoff
        end
    end
    | Index(ei, off') -> begin
        let telem = match unrollType t with 
          TArray(telem, _, _) -> telem
        | _ -> E.s (bug "Simplify: simplifyLval: index on a non-array")
        in
        let next, restoff = offsetToInt telem off' in
        add 
          (BinOp(Mult, ei, SizeOf telem, !upointType)) 
          next,
        restoff
    end
  in
  let tres = TPtr(typeOfLval lv, []) in
  let typeForCast restOff: typ =
    (* in (e+i)-> restoff, what should we cast e+i to? *)
    match restOff with
      Index _ -> E.s (bug "index in restOff")
    | NoOffset -> tres
    | Field(fi, NoOffset) -> (* bitfield *)
        TPtr(TComp(fi.fcomp, []), [])
    | Field(fi, _) -> E.s (bug "bug in offsetToInt")
  in
  match lv with 
    Mem a, off when not !convertFieldOffsets ->
      let a' = if !simpleMem then makeBasic setTemp a else a in
      Mem a', off
  | Mem a, off ->
      let offidx, restoff = offsetToInt (typeOfLval (Mem a, NoOffset)) off in
      let a' = 
        if offidx <> zero then 
          add (mkCast a !upointType) offidx
        else
          a
      in
      let a' = if !simpleMem then makeBasic setTemp a' else a' in
      Mem (mkCast a' (typeForCast restoff)), restoff
  (* We are taking this variable's address; but suppress simplification if it's a simple function
   * call in no-convert-function-calls mode*)
  | Var v, off when v.vaddrof && (!convertDirectCalls || not (isFunctionType (typeOfLval lv) ))  ->
      if (not !convertFieldOffsets) then (Var v, off) else
      let offidx, restoff = offsetToInt v.vtype off in
      (* We cannot call makeBasic recursively here, so we must do it 
       * ourselves *)
      let a = mkAddrOrStartOf (Var v, NoOffset) in
      let a' = 
        if offidx = zero then a else
        if !simpleMem then
	        add (mkCast a !upointType) (makeBasic setTemp offidx)
	    else add (mkCast a !upointType) offidx
      in
      let a' = if !simpleMem then setTemp a' else a' in
      Mem (mkCast a' (typeForCast restoff)), restoff

  | Var v, off -> 
      (Var v, simplifyOffset setTemp off)


(* Simplify an offset and make sure it has only three address expressions in 
 * indices *)
and simplifyOffset (setTemp: taExp -> bExp) = function
    NoOffset -> NoOffset
  | Field(fi, off) -> Field(fi, simplifyOffset setTemp off)
  | Index(ei, off) -> 
      let ei' = makeBasic setTemp ei in
      Index(ei', simplifyOffset setTemp off)




(** This is a visitor that will turn all expressions into three address code *)
class threeAddressVisitor (fi: fundec) = object (self)
  inherit nopCilVisitor

  method private makeTemp (e1: exp) : exp = 
    let t = makeTempVar fi (typeOf e1) in
    (* Add this instruction before the current statement *)
    self#queueInstr [Set(var t, e1, !currentLoc)];
    Lval(var t)

      (* We'll ensure that this gets called only for top-level expressions 
       * inside functions. We must turn them into three address code. *)
  method vexpr (e: exp) = 
    let e' = makeThreeAddress self#makeTemp e in
    ChangeTo e'


     (** We want the argument in calls to be simple variables *)
  method vinst (i: instr) =
    match i with 
      Call (someo, f, args, loc) -> 
        let someo' = 
          match someo with 
            Some lv -> Some (simplifyLval self#makeTemp lv)
          | _ -> None
        in
        let f' = makeBasic self#makeTemp f in
        let args' = Util.list_map (makeBasic self#makeTemp) args in 
        ChangeTo [ Call (someo', f', args', loc) ]
  | _ -> DoChildren

      (* This method will be called only on top-level "lvals" (those on the 
       * left of assignments and function calls) *)
  method vlval (lv: lval) = 
    ChangeTo (simplifyLval self#makeTemp lv)
end


(* Whether to split the arguments of functions *)
let splitArguments = true

(* Whether we try to do the splitting all in one pass. The advantage is that 
 * it is faster and it generates nicer names *)
let lu = locUnknown

(* Go over the code and split some temporary variables of stucture type into 
 * several separate variables. The hope is that the compiler will have an 
 * easier time to do standard optimizations with the resulting scalars *)
(* Unfortunately, implementing this turns out to be more complicated than I 
 * thought *)

(** Iterate over the fields of a structured type. Returns the empty list if 
 * no splits. The offsets are in order in which they appear in the structure 
 * type. Along with the offset we pass a string that identifies the 
 * meta-component, and the type of that component. *)
let rec foldRightStructFields
    (doit: offset -> string -> typ -> 'a) (* Invoked on non-struct fields *)
    (off: offset)
    (post: 'a list) (** A suffix to what you compute *)
    (fields: fieldinfo list) : 'a list = 
  List.fold_right
    (fun f post -> 
      let off' = addOffset (Field(f, NoOffset)) off in 
      match unrollType f.ftype with 
        TComp (comp, _) when comp.cstruct -> (* struct type: recurse *)
          if (List.exists (fun f -> isArrayType f.ftype) comp.cfields) then
            begin
              E.log ("%a:  Simplify: Not splitting struct %s because one"
                     ^^" of its fields is an array.\n") 
                d_loc (List.hd comp.cfields).floc
                comp.cname;
              (doit off' f.fname f.ftype) :: post
            end
          else
            foldRightStructFields doit off' post comp.cfields
      | _ -> 
          (doit off' f.fname f.ftype) :: post)
    fields
    post
  

let rec foldStructFields
    (t: typ) 
    (doit: offset -> string -> typ -> 'a) 
    : 'a list = 
  match unrollType t with 
    TComp (comp, _) when comp.cstruct -> 
      foldRightStructFields doit NoOffset [] comp.cfields
  | _ -> []
      
      
(* Map a variable name to a list of component variables, along with the 
 * accessor offset. The fields are in the order in which they appear in the 
 * structure. *)
let newvars : (string, (offset * varinfo) list) H.t = H.create 13

(* Split a variable and return the replacements, in the proper order. If this 
 * variable is not split, then return just the variable. *)
let splitOneVar (v: varinfo) 
                (mknewvar: string -> typ -> varinfo) : varinfo list = 
  try 
    (* See if we have already split it *)
    Util.list_map snd (H.find newvars v.vname)
  with Not_found -> begin
    let vars: (offset * varinfo) list = 
      foldStructFields v.vtype 
        (fun off n t -> (* make a new one *)
          let newname = v.vname ^ "_" ^ n in 
          let v'= mknewvar newname t in
          (off, v'))
    in
    if vars = [] then
      [ v ]
    else begin
      (* Now remember the newly created vars *)
      H.add newvars v.vname vars;
      Util.list_map snd vars (* Return just the vars *)
    end
  end


(* A visitor that finds all locals that appear in a call or have their 
 * address taken *)
let dontSplitLocals : (string, bool) H.t = H.create 111
class findVarsCantSplitClass : cilVisitor = object (self) 
  inherit nopCilVisitor
        
        (* expressions, to see the address being taken *)
  method vexpr (e: exp) : exp visitAction =
    match e with 
      AddrOf (Var v, NoOffset) -> 
        H.add dontSplitLocals v.vname true; SkipChildren
      (* See if we take the address of the "_ms" field in a variable *)
    | _ -> DoChildren


          (* variables involved in call instructions *)
  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Call (res, f, args, _) -> 
        (match res with 
          Some (Var v, NoOffset) -> H.add dontSplitLocals v.vname true
        | _ -> ());
        if not splitArguments then 
          List.iter (fun a -> 
            match a with
              Lval (Var v, NoOffset) -> H.add dontSplitLocals v.vname true
            | _ -> ()) args; 
        (* Now continue the visit *)
        DoChildren

    | _ -> DoChildren

          (* Variables used in return should not be split *)
  method vstmt (s: stmt) : stmt visitAction = 
    match s.skind with 
      Return (Some (Lval (Var v, NoOffset)), _) -> 
        H.add dontSplitLocals v.vname true; DoChildren
    | Return (Some e, _) -> 
        DoChildren
    | _ -> DoChildren

  method vtype t = SkipChildren

end
let findVarsCantSplit = new findVarsCantSplitClass

let isVar lv =
  match lv with 
      (Var v, NoOffset) -> true
    | _ -> false


class splitVarVisitorClass(func:fundec option) : cilVisitor = object (self)
  inherit nopCilVisitor

  method private makeTemp (e1: exp) : exp = 
    let fi:fundec = match func with
        Some f -> f
      | None -> 
          E.s (bug "You can't create a temporary if you're not in a function.")
    in
    let t = makeTempVar fi (typeOf e1) in
    (* Add this instruction before the current statement *)
    self#queueInstr [Set(var t, e1, !currentLoc)];
    Lval(var t)


  (* We must process the function types *)
  method vtype t = 
    (* We invoke the visitor first and then we fix it *)
    let postProcessFunType (t: typ) : typ = 
      match t with 
        TFun(rt, Some params, isva, a) -> 
          let rec loopParams = function
              [] -> []
            | ((pn, pt, pa) :: rest) as params -> 
                let rest' = loopParams rest in
                let res: (string * typ * attributes) list = 
                  foldStructFields pt
                    (fun off n t -> 
                      (* Careful with no-name parameters, or we end up with 
                       * many parameters named _p ! *)
                      ((if pn <> "" then pn ^ n else ""), t, pa)) 
                in
                if res = [] then (* Not a fat *)
                  if rest' == rest then 
                    params (* No change at all. Try not to reallocate so that 
                            * the visitor does not allocate. *)
                  else
                    (pn, pt, pa) :: rest'
                else (* Some change *)
                  res @ rest'
          in
          let params' = loopParams params in
          if params == params' then 
            t
          else
            TFun(rt, Some params', isva, a)
          
      | t -> t
    in
    if splitArguments then 
      ChangeDoChildrenPost(t, postProcessFunType)
    else
      SkipChildren

      (* Whenever we see a variable with a field access we try to replace it 
       * by its components *)
  method vlval ((b, off) : lval) : lval visitAction = 
    try
      match b, off with
        Var v, (Field _ as off) ->
          (* See if this variable has some splits.Might throw Not_found *)
          let splits = H.find newvars v.vname in
          (* Now find among the splits one that matches this offset. And 
           * return the remaining offset *)
          let rec find = function
              [] -> 
                E.s (E.bug "Cannot find component %a of %s\n" 
                       (d_offset nil) off v.vname)
            | (splitoff, splitvar) :: restsplits -> 
                let rec matches = function 
                    Field(f1, rest1), Field(f2, rest2) 
                      when f1.fname = f2.fname -> 
                        matches (rest1, rest2)
                  | off, NoOffset -> 
                      (* We found a match *)
                      (Var splitvar, off)
                  | NoOffset, restoff -> 
                      ignore (warn "Found aggregate lval %a" 
                                d_lval (b, off));
                      find restsplits

                  | _, _ -> (* We did not match this one; go on *)
                      find restsplits
                in
                matches (off, splitoff)
          in
          ChangeTo (find splits)
      | _ -> DoChildren
    with Not_found -> DoChildren

        (* Sometimes we pass the variable as a whole to a function or we 
         * assign it to something *)
  method vinst (i: instr) : instr list visitAction = 
    match i with
      (* Split into several instructions and then do children inside
       * the rhs.  Howver, v might appear in the rhs and if we
       * duplicate the instruction we might get bad
       * results. (e.g. test/small1/simplify_Structs2.c). So first copy
       * the rhs to temp variables, then to v. 
       *
       * Optimization: if the rhs is a variable, skip the temporary vars.
       * Either the rhs = lhs, in which case this is all a nop, or it's not, 
       * in which case the rhs and lhs don't overlap.*)

      Set ((Var v, NoOffset), Lval lv, l) when H.mem newvars v.vname -> begin
        let needTemps = not (isVar lv) in
        let vars4v = H.find newvars v.vname in
        if vars4v = [] then E.s (errorLoc l "No fields in split struct");
        ChangeTo 
          (Util.list_map 
             (fun (off, newv) ->  
                let lv' = 
                  visitCilLval (self :> cilVisitor)
                    (addOffsetLval off lv) in
                (* makeTemp creates a temp var and puts (Lval lv') in it,
                   before any instructions in this ChangeTo list are handled.*)
                let lv_tmp = if needTemps then
                               self#makeTemp (Lval lv') 
                             else 
                               (Lval lv')
                in
                Set((Var newv, NoOffset), lv_tmp, l))
             vars4v)
      end 
 
      | Set (lv, Lval (Var v, NoOffset), l) when H.mem newvars v.vname -> begin
          (* Split->NonSplit assignment.  no overlap between lhs and rhs 
             is possible*)
          let vars4v = H.find newvars v.vname in
          if vars4v = [] then E.s (errorLoc l "No fields in split struct");
          ChangeTo  
            (Util.list_map 
               (fun (off, newv) -> 
                  let lv' = 
                    visitCilLval (self :> cilVisitor)
                      (addOffsetLval off lv) in
                  Set(lv', Lval (Var newv, NoOffset), l))
               vars4v)
        end 

        (* Split all function arguments in calls *)
      | Call (ret, f, args, l) when splitArguments ->
          (* Visit the children first and then see if we must change the 
           * arguments *)
          let finishArgs = function
              [Call (ret', f', args', l')] as i' -> 
                let mustChange = ref false in
                let newargs = 
                  (* Look for opportunities to split arguments. If we can
                   * split, we must split the original argument (in args).
                   * Otherwise, we use the result of processing children
                   * (in args'). *)
                  List.fold_right2
                    (fun a a' acc -> 
                      match a with
                        Lval (Var v, NoOffset) when H.mem newvars v.vname -> 
                          begin
                            mustChange := true;
                            (Util.list_map 
                               (fun (_, newv) -> 
                                 Lval (Var newv, NoOffset)) 
                               (H.find newvars v.vname))
                            @ acc
                          end
                      | Lval lv  -> begin
                          let newargs = 
                            foldStructFields (typeOfLval lv)
                              (fun off n t ->
                                 let lv' = addOffsetLval off lv in
                                 Lval lv') in
                          if newargs = [] then
                            a' :: acc (* not a split var *)
                          else begin
                            mustChange := true;
                            newargs @ acc
                          end
                        end
                      | _ -> (* only lvals are split, right? *)
                          a' :: acc)
                    args args'
                    []
                in
                if !mustChange then 
                  [Call (ret', f', newargs, l')]
                else
                  i'
            | _ -> E.s (E.bug "splitVarVisitorClass: expecting call")
          in
          ChangeDoChildrenPost ([i], finishArgs)

      | _ -> DoChildren

        
  method vfunc (func: fundec) : fundec visitAction = 
    H.clear newvars;
    H.clear dontSplitLocals;
    (* Visit the type of the function itself *)
    if splitArguments then 
      func.svar.vtype <- visitCilType (self :> cilVisitor) func.svar.vtype;

    (* Go over the block and find the candidates *)
    ignore (visitCilBlock findVarsCantSplit func.sbody);

    (* Now go over the formals and create the splits *)
    if splitArguments then begin
      (* Split all formals because we will split all arguments in function 
       * types *)
      let newformals = 
        List.fold_right 
          (fun form acc -> 
            (* Process the type first *)
            form.vtype <- 
               visitCilType (self : #cilVisitor :> cilVisitor) form.vtype;
            let form' = 
              splitOneVar form 
                          (fun s t -> makeTempVar func ~insert:false ~name:s t)
            in
            (* Now it is a good time to check if we actually can split this 
             * one *)
            if List.length form' > 1 &&
               H.mem dontSplitLocals form.vname then
              ignore (warn "boxsplit: can't split formal \"%s\" in %s. Make sure you never take the address of a formal."
                     form.vname func.svar.vname);
            form' @ acc)
          func.sformals [] 
      in
      (* Now make sure we fix the type.  *)
      setFormals func newformals
    end;
    (* Now go over the locals and create the splits *)
    List.iter 
      (fun l ->
        (* Process the type of the local *)
        l.vtype <- visitCilType (self :> cilVisitor) l.vtype;
        (* Now see if we must split it *)
        if not (H.mem dontSplitLocals l.vname) then begin
          ignore (splitOneVar l (fun s t -> makeTempVar func ~name:s t))
        end) 
      func.slocals;
    (* Now visit the body and change references to these variables *)
    ignore (visitCilBlock (self :> cilVisitor) func.sbody);
    H.clear newvars;
    H.clear dontSplitLocals;
    SkipChildren  (* We are done with this function *)

  (* Try to catch the occurrences of the variable in a sizeof expression *)
  method vexpr (e: exp) = 
    match e with 
    | SizeOfE (Lval(Var v, NoOffset)) -> begin
        try
          let splits =  H.find newvars v.vname in
          (* We cound here on no padding between the elements ! *)
          ChangeTo
            (List.fold_left
               (fun acc (_, thisv) -> 
                 BinOp(PlusA, SizeOfE(Lval(Var thisv, NoOffset)), 
                       acc, uintType))
               zero
               splits)
        with Not_found -> DoChildren
    end
    | _ -> DoChildren
end

let doGlobal = function 
    GFun(fi, _) ->  
      (* Visit the body and change all expressions into three address code *)
      let v = new threeAddressVisitor fi in
      fi.sbody <- visitCilBlock v fi.sbody;
      if !splitStructs then begin
        H.clear dontSplitLocals;
        let splitVarVisitor = new splitVarVisitorClass (Some fi) in    
        ignore (visitCilFunction splitVarVisitor fi);
      end
  | GVarDecl(vi, _) when isFunctionType vi.vtype ->
      (* we might need to split the args/return value in the function type. *)
      if !splitStructs then begin
        H.clear dontSplitLocals;
        let splitVarVisitor = new splitVarVisitorClass None in    
        ignore (visitCilVarDecl splitVarVisitor vi);
      end
  | _ -> ()
      
let feature = 
  { fd_name = "simplify";
    fd_enabled = false;
    fd_description = "compiles CIL to 3-address code";
    fd_extraopt = [
      ("--no-split-structs", Arg.Clear splitStructs,
                    " do not split structured variables"); 
      ("--no-convert-direct-calls", Arg.Clear convertDirectCalls,
                    " do not convert direct function calls to function pointer \
                      calls if the address of the function was taken");
      ("--no-convert-field-offsets", Arg.Unit ( fun () ->
                      convertFieldOffsets := false;
                      (* do not split structs in function calls *)
                      splitStructs := false
                    ),
                    " do not convert field offsets to offsets by integer.    \
                      Implies --no-split-structs.  To be used by static code \
                      verification tools.");
    ];
    fd_doit = (function f -> iterGlobals f doGlobal);
    fd_post_check = true;
}

let () = Feature.register feature
