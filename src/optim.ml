(* Written by S. P. Rahul *)

(* Optimizes the placement of boxing code *)

open Pretty
open Cil
module E=Errormsg

let debug = true

(*-----------------------------------------------------------------*)
(* Optimizing CHECK_NULLs *)
(* This is a simple intraprocedural analysis.
   -- Maintain a list of expressions that are guaranteed to
      be non-null because of a preceding check
   -- Invalidate this list when a function call (to a function
      other than CHECK_? ), label, start of loop or end of loop 
      is encountered
   -- Also invalidate on assignment
   -- Merge the results of the branches of if-then-else
   -- (TODO) Take predicates in if-then-else statements of the form
      "if (e)..." into account
*)
    
(* Take a stmt  and a list of lval (nnl means NotNullList) guaranteed 
   to be non-null. Return (s',l') where s is the optimized s' and l'
   is the updated l *)
let rec nullChecksOptimStmt (s:Cil.stmt) (nnl:Cil.lval list) =
  match s with
    Skip -> (s,nnl)
  | Sequence(l) -> 
      let (l',nnl') = nullChecksOptimSeq l nnl in (mkSeq l',nnl')
  | Loops(s) -> 
      let (s',_) = nullChecksOptimStmt s [] in (Loops(s'),[])
  | IfThenElse(e,s1,s2,l) ->
      (* TODO: consider the predicate *)
      let (s1',nnl1) = nullChecksOptimStmt s1 nnl in
      let (s2',nnl2) = nullChecksOptimStmt s2 nnl in
      (IfThenElse(e,s1',s2',l),
       List.filter (function x -> List.mem x nnl2) nnl1)
  | Label(_) | Gotos(_) | Returns(_) -> (s,nnl)
  | Switchs(_,_,_) | Case(_) | Default -> 
      (s,nnl)
(*      E.s (E.unimp "OPTIM cannot handle switch, case, default yet")*)
  | Break | Continue -> (s,nnl)
  | Instr(i, l) -> (match i with
      Call(_,Lval(Var x,_),args) when x.vname = "CHECK_NULL" ->
        (* args must be a list of one element -- the exp which we
           have to ensure is non-null *)
        (* if the arg is not a (possibly casted) lval, we don't
           try to remove it *)
        let arg = List.hd args in
        (match arg with
          CastE(_,Lval _) | Lval _ ->  
            let checkLval = 
              (match arg with
                CastE(_,Lval e) -> e (* Remove the cast, if any *)
              | Lval e -> e
              | _ -> E.s (E.bug "Should not reach here\n"))
            in
            if (List.mem checkLval nnl) then
              (Skip,nnl)
            else 
              (s,checkLval::nnl)
        | _ -> (s,nnl))
    | Call(_,Lval(Var x,_),args) 
      when ((String.length x.vname) > 6 && 
            (String.sub x.vname 0 6)="CHECK_") -> (s,nnl)
    | Call _ -> (s,[])
    | Asm _ -> (s,nnl) 
    | Set(lv,e) -> (* remove the lv from nnl *)
        (s,List.filter (function x -> x<>lv) nnl))
                
and nullChecksOptimSeq l nnl =
  match l with 
    [] -> ([],nnl) 
  | (hd::tl) -> 
      let (hd',nnl') = nullChecksOptimStmt hd nnl in
      let (tl',nnl'') = nullChecksOptimSeq tl nnl' in
      (hd'::tl',nnl'')

   
(*------------------------------------------------------------*)
let nullChecksOptim f =
  let (newBody,_) = nullChecksOptimStmt f.sbody [] in
  f.sbody <-  newBody;
  f
(*------------------------------------------------------------*)
(* Carry out a series of optimizations on a function definition *)

let optimFun f =
  if debug then
    ignore (printf "Analyzing %s\n" f.svar.vname);

  let optimizedF = ref f in
  (* Carry out the optimizations, one at a time *)
  
  (* Remove redundant CHECK_NULL *)
  optimizedF := nullChecksOptim !optimizedF;
  
  (* Return the final optimized version *)
  !optimizedF
    
(*------------------------------------------------------------*)
let optimFile file =
  if debug
  then begin
    ignore(printf "\n-------------------------------------------------\n");
    ignore(printf "OPTIM MODULE STARTS\n\n")
  end;
  
  (* Replace every function definition by its optimized version*)
  file.globals <- List.map 
      (function 
          GFun(f,l) -> GFun(optimFun f, l)
        | _ as other -> other)
      file.globals;
  
  if debug
  then begin
    ignore(printf "\n\nOPTIM MODULE ENDS\n");
    ignore(printf "--------------------------------------------------\n")
  end;
  
  file
(*------------------------------------------------------------*)
    

  
