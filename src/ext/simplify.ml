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
 - all field and index computations are turned into address arithmetic

*)


open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

type taExp = exp (* Three address expression *)
type bExp = exp  (* Basic expression *)

(* Turn an expression into a three address expression (and queue some 
 * instructions in the process) *)
let rec makeThreeAddress 
    (setTemp: taExp -> bExp) (* Given an expression save it into a temp and 
                              * return that temp *)
    (e: exp) : taExp = 
  match e with 
    SizeOf _ | SizeOfE _ | AlignOf _ |  AlignOfE _ | SizeOfStr _ -> 
      constFold true e
  | Const _ | AddrOf (Var _, NoOffset) -> e
  | Lval lv -> Lval (simplifyLval setTemp lv)
  | BinOp(bo, e1, e2, tres) -> 
      BinOp(bo, makeBasic setTemp e1, makeBasic setTemp e2, tres)
  | UnOp(uo, e1, tres) -> 
      UnOp(uo, makeBasic setTemp e1, tres)
  | CastE(t, e) -> 
      CastE(t, makeBasic setTemp e)
  | AddrOf lv -> begin
      match simplifyLval setTemp lv with 
        Mem a, NoOffset -> a
      | _ -> (* This is impossible, because we are taking the address 
          * of v and simplifyLval should turn it into a Mem *)
          E.s (bug "Simplify: makeThreeAddress for AddrOf")
  end
  | StartOf lv -> 
      makeThreeAddress setTemp (AddrOf (addOffsetLval (Index(zero, NoOffset))
                                          lv))

(* Make a basic expression *)      
and makeBasic (setTemp: taExp -> bExp) (e: exp) : bExp = 
  (* Make it a three address expression first *)
  let e' = makeThreeAddress setTemp e in
  (* See if it is a basic one *)
  match e' with 
    Const _ | Lval (Var _, _) 
  | AddrOf (Var _, NoOffset) | StartOf (Var _, NoOffset) -> e'
  | SizeOf _ | SizeOfE _ | AlignOf _ |  AlignOfE _ | SizeOfStr _ -> 
      E.s (bug "Simplify: makeBasic found SizeOf")
  | _ -> setTemp e' (* Put it into a temporary otherwise *)


and simplifyLval 
    (setTemp: taExp -> bExp) 
    (lv: lval) : lval = 
  (* Add, watching for a zero *)
  let add (e1: exp) (e2: exp) = 
    if isZero e2 then e1 else BinOp(PlusA, e1, e2, !upointType) 
  in
  (* Convert an offset to an integer *)
  let rec offsetToInt 
      (t: typ) (* The type of the host *)
      (off: offset) : exp = 
    match off with 
      NoOffset -> zero
    | Field(fi, off') -> begin
        try 
          let start, _ = bitsOffset t (Field(fi, NoOffset)) in
          if start land 7 <> 0 then 
            E.s (unimp "Field does not start on a byte boundary");
          add (integer (start / 8)) (offsetToInt fi.ftype off')
        with SizeOfError _ -> 
          E.s (unimp "Cannot compute the sizeof")
    end
    | Index(ei, off') -> begin
        let telem = match unrollType t with 
          TArray(telem, _, _) -> telem
        | _ -> E.s (bug "Simplify: simplifyLval: index on a non-array")
        in
        add 
          (BinOp(Mult, ei, SizeOf telem, !upointType)) 
          (offsetToInt telem off')
    end
  in
  let tres = TPtr(typeOfLval lv, []) in
  match lv with 
    Mem a, off -> 
      let off' = offsetToInt (typeOfLval (Mem a, NoOffset)) off in
      let a' = makeBasic setTemp (add (mkCast a !upointType) off') in
      Mem (mkCast a' tres), NoOffset

  | Var v, off when v.vaddrof -> (* We are taking this variable's address *)
      let off' = offsetToInt v.vtype off in
      (* We cannot call makeBasic recursively here, so we must do it 
       * ourselves *)
      let off'' = makeBasic setTemp off' in
      let a' = setTemp 
          (add (mkCast (mkAddrOrStartOf (Var v, NoOffset))
                  !upointType) off'') 
      in
      Mem (mkCast a' tres), NoOffset

  | Var v, off -> (Var v, simplifyOffset setTemp off)


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

      (* We'll ensure that this gets called only for top-level expressions 
       * inside functions. We must turn them into three address code. *)
  method vexpr (e: exp) = 
    let e' = 
      makeThreeAddress 
        (fun e1 -> 
          let t = makeTempVar fi (typeOf e1) in
          (* Add this instruction before the current statement *)
          self#queueInstr [Set(var t, e1, !currentLoc)];
          Lval(var t)) 
        e
    in
    ChangeTo e'

      (* This method will be called only on top-level "lvals" (those on the 
       * left of assignments and function calls) *)
  method vlval (lv: lval) = 
    ChangeTo 
      (simplifyLval 
         (fun e1 -> 
           let t = makeTempVar fi (typeOf e1) in
           (* Add this instruction before the current statement *)
             self#queueInstr [Set(var t, e1, !currentLoc)];
           Lval(var t)) 
         lv)
end

let rec doOneFunction (fi: fundec) = 
  (* Visit the body and change all expressions into three address code *)
  let v = new threeAddressVisitor fi in
  fi.sbody <- visitCilBlock v fi.sbody;
  ()

let feature : featureDescr = 
  { fd_name = "simplify";
    fd_enabled = ref false;
    fd_description = "compiles CIL to 3-address code";
    fd_extraopt = [];
    fd_doit = 
    (function f -> iterGlobals f 
        (function GFun(fi, _) -> doOneFunction fi | _ -> ()));
    fd_post_check = true;
}

