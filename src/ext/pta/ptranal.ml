(*
 *
 * Copyright (c) 2001-2002, 
 *  John Kodumal        <jdokumal@eecs.berkeley.edu>
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
(*
  todo
  
  -- varargs 
  
*)
exception Bad_return
exception Bad_function

open Cil

module H = Hashtbl

module A = Golf 

(* module A = Steensgaard *)

(*
module A = Dummy
*) 

type access = A.lvalue * bool

type access_map = (lval, access) H.t

(***********************************************************************)
(*                                                                     *)
(* Global Variables                                                    *)
(*                                                                     *)
(***********************************************************************)

let print_constraints = A.print_constraints
let debug_constraints = A.debug_constraints
let debug_aliases = A.debug_aliases
let smart_aliases = A.smart_aliases
let debug = A.debug
let analyze_mono = A.analyze_mono
let no_flow = A.no_flow
let no_sub = A.no_sub
let fun_ptrs_as_funs = ref false
let show_progress = ref false

let current_fundec : fundec option ref = ref None

let fun_access_map : (fundec, access_map) H.t = H.create 64

let current_ret : A.tau option ref = ref None

let lvalue_hash : (varinfo,A.lvalue) H.t = H.create 64

let count : int ref = ref 0

let fresh_index () : int = 
  incr count;
  !count
  
let alloc_names = ["malloc";"calloc";"realloc";"xmalloc";"__builtin_alloca";
		   "alloca";"kmalloc"]

let all_globals : varinfo list ref = ref []
let all_functions : fundec list ref = ref []

(***********************************************************************)
(*                                                                     *)
(* Utility Functions                                                   *)
(*                                                                     *)
(***********************************************************************)

let pointer_destroying_unop op : bool =
  false

let pointer_destroying_binop op : bool = 
  match op with
    | PlusA -> false                             
    | PlusPI -> false                            
    | IndexPI -> false                           
    | MinusA -> false                             
    | MinusPI -> false                             
    | MinusPP -> false                              
    | Mod -> false         
    | BXor -> false     
    | _ -> true

let is_alloc_fun = function 
    | Lval (lh,o) ->
	if (isFunctionType (typeOfLval (lh,o))) 
	then
	  match lh with
	    | Var v -> List.mem v.vname alloc_names
	    | _ -> false
	else
	  false
    | _ -> false

let next_alloc () = 
  let name = "alloc_" ^ string_of_int (fresh_index())
  in A.address (A.make_lvalue false name) (* check *)

(***********************************************************************)
(*                                                                     *)
(* AST Traversal Functions                                             *)
(*                                                                     *)
(***********************************************************************)

(* should do nothing, might need to worry about Index case *)
(* let analyzeOffset (o : offset ) : A.tau = A.bottom () *)

let analyze_var_decl (v : varinfo ) : A.lvalue =
  try
    H.find lvalue_hash v
  with
    | Not_found -> 
	begin
	  let is_global = 
	    if (isFunctionType(v.vtype)) then false else v.vglob  
	  in
	  let lv = A.make_lvalue is_global v.vname 
	  in
	    H.add lvalue_hash v lv;
	    lv
	end

let isFunPtrType (t : typ) : bool = 
  match t with
    | TPtr (t,_) ->
	isFunctionType t
    | _ -> false
  
let rec analyze_lval (lv : lval ) : A.lvalue =
  match lv with
    | Var v,_ -> (* instantiate every syntactic occurrence of a function *)
	begin
	  let alv = 
	    if (isFunctionType (typeOfLval lv)) 
	    then A.instantiate (analyze_var_decl v) (fresh_index())
	    else analyze_var_decl v
	  in
	    match (!current_fundec) with
	      | None -> alv
	      | Some f ->
		  let accesses = H.find fun_access_map f in
		    if (H.mem accesses lv) then alv
		    else
		      begin
			H.add accesses lv (alv,true);
			alv
		      end
	end
    | Mem e,_ -> 
	begin 
	  (* assert (not (isFunctionType(typeOf(e))) ); *)
	  let alv = 
	    if (!fun_ptrs_as_funs && isFunPtrType (typeOf e))
	    then
	      analyze_expr_as_lval e
	    else
	      A.deref (analyze_expr e)
	  in
	    match (!current_fundec) with
	      | None -> alv
	      | Some f ->
		  let accesses = H.find fun_access_map f in
		    if (H.mem accesses lv) 
		    then alv
		    else
		      begin
			H.add accesses lv (alv,false);
			alv
		      end
	end
	
and analyze_expr_as_lval (e : exp) : A.lvalue = 
  match e with
    | Lval l -> analyze_lval l 
    | _ -> assert(false) (* todo -- other kinds of expressions? *)

and analyze_expr (e : exp ) : A.tau = 
  match e with
    | Const c -> A.bottom()
    | Lval l ->	A.rvalue (analyze_lval l)
    | SizeOf _ -> A.bottom()
    | SizeOfStr _ -> A.bottom()
    | AlignOf _ -> A.bottom()
    | UnOp (op,e,t) -> 
	begin
	  if (pointer_destroying_unop op)
	  then 
	    A.bottom ()
	  else
	    analyze_expr e
	end
    | BinOp (op,e,e',t) ->
	begin
	  if (pointer_destroying_binop op)
	  then
	    A.bottom ()
	  else
	    A.join (analyze_expr e) (analyze_expr e')
	end
    | CastE (t,e) ->
	analyze_expr(e)
    | AddrOf l ->  
	if (!fun_ptrs_as_funs && isFunctionType (typeOfLval(l)) ) then
	  A.rvalue (analyze_lval l)
	else
	  A.address (analyze_lval l)
    | StartOf l -> A.address (analyze_lval l)
    | AlignOfE _ -> A.bottom()
    | SizeOfE _ -> A.bottom() 

(* check *)
let rec analyze_init (i : init ) : A.tau = 
  match i with 
    | SingleInit e ->
	analyze_expr(e)
    | CompoundInit (t,oi) ->
	A.join_inits (List.map (function (_,i) -> analyze_init(i)) oi)

let analyze_instr (i : instr ) : unit =
  match i with
    | Set (lval,rhs,l) -> 
	A.assign (analyze_lval lval) (analyze_expr rhs)
    | Call (res,fexpr,actuals,l) ->
	if ( not (isFunctionType(typeOf(fexpr))) ) 
	then () (* varargs ?? *)
	else if (is_alloc_fun fexpr)
	then 
	  match res with
	    | Some r -> A.assign (analyze_lval r) (next_alloc())
	    | None -> ()
	else
	  begin
	    let 
	      fnres,site = A.apply (analyze_expr fexpr) 
			     (List.map analyze_expr actuals)
	    in
	      match res with 
		| Some r ->
		    A.assign_ret site (analyze_lval (r)) fnres
		| None -> ()
	  end
    | Asm _ -> ()
	
let rec analyze_stmt (s : stmt ) : unit = 
  match s.skind with
    | Instr il -> List.iter analyze_instr il
    | Return (eo,l) ->
	begin
	  match eo with
	    | Some e ->
		begin
		  match (!current_ret)
		  with
		    | Some ret ->
			A.return (ret) (analyze_expr e)
		    | None -> raise Bad_return
		end
	    | None -> ()
	end
    | Goto (s',l) ->() (* analyze_stmt(!s') *)
    | If (e,b,b',l) ->
	begin
	  (* ignore the expression e; expressions can't be side-effecting *)
	  analyze_block b;
	  analyze_block b'
	end
    | Switch (e,b,sl,l) ->
	begin
	  analyze_block b;
	  List.iter analyze_stmt sl
	end
    | Loop (b,l,_,_) -> analyze_block b
    | Block b -> analyze_block(b)
    | Break l -> ()
    | Continue l -> ()
	

and analyze_block (b : block ) : unit = 
  List.iter analyze_stmt b.bstmts

let analyze_function (f : fundec ) : unit = 
  let oldlv = analyze_var_decl f.svar in
  let ret = A.make_fresh (f.svar.vname ^ "_ret") in
  let formals = List.map analyze_var_decl f.sformals in
  let locals = List.map analyze_var_decl f.slocals in
  let newf = A.make_function (f.svar.vname) formals ret in
    begin 
      if (!show_progress) 
      then
	begin (*
	  if (f.svar.vname = "jpeg_write_scanlines") then
	    begin
	      A.debug_constraints := true;
	      A.print_constraints := true;
	    end;*)
	  Printf.printf "Analyzing function %s" f.svar.vname;
	  print_newline()
	end;
      current_fundec := Some f;
      H.add fun_access_map f (H.create 8);
      A.assign oldlv newf;
      current_ret := Some ret;
      analyze_block(f.sbody)
    end

let analyze_global (g : global ) : unit = 
  match g with 
    | GVarDecl (v,l) -> (* ignore (analyze_var_decl(v)) -- no need *) ()
    | GVar (v,init,l) -> 
	begin
	  all_globals := v :: (!all_globals);
	  match init with
	    | Some i ->
		A.assign (analyze_var_decl(v)) (analyze_init(i))
	    | None ->
		ignore (analyze_var_decl(v))
	end
    | GFun (f,l) ->
	begin
	  all_functions := f :: (!all_functions);
	  analyze_function(f)
	end
    | _ -> ()

let analyze_file (f : file) : unit = 
  iterGlobals f analyze_global 


let count_hash_elts h =
  let result = ref 0 in
    begin
      H.iter (fun _ -> fun _ -> incr result) lvalue_hash;
      !result
    end
  
let show_progress_fn (counted : int ref) (total : int) : unit = 
  begin
    incr counted;
    if (!show_progress) then
      begin
	Printf.printf "Computed flow for %d of %d sets" (!counted) total;
	print_newline()
      end
    else ()
  end

let compute_results (show_sets : bool) : unit = 
  print_string "Computing points-to sets...";
  print_newline();
  let 
    total_pointed_to = ref 0
  in
  let
    total_lvalues = count_hash_elts lvalue_hash
  in
  let 
    counted_lvalues = ref 0
  in
  let global_lvalues = ref 0
  in
  let print_result (name,set) =
      let rec print_set s = 
	match s with
	  | h :: [] -> print_string h
	  | h :: t -> print_string (h ^ ", "); print_set t 
	  | [] -> ()
      in
	total_pointed_to := !total_pointed_to + (List.length set);
	if (show_sets) then
	  begin
	    let ptsize = List.length set in
	      if (ptsize > 0) then
		begin
		  print_string 
		    (name ^ "(" ^ (string_of_int ptsize) ^ ") -> ");
		  print_set set;
		  print_newline ()
		end
	  end
	else ()
  in
  let lval_elts : (string * (string list)) list ref = ref [] 
  in 
    Hashtbl.iter (fun vinf -> fun lv -> 
		    begin
		      if (A.global_lvalue lv) then incr global_lvalues;
		      (show_progress_fn counted_lvalues total_lvalues);
		      lval_elts := (vinf.vname, A.points_to lv) :: (!lval_elts)
		    end
		 ) lvalue_hash;
    List.iter print_result (!lval_elts); 
    Printf.printf "Total number of things pointed to: %d\n" !total_pointed_to
    (* 
       Printf.printf "Total number of inferred globals : %d\n" !global_lvalues
    *)
  
let print_types () : unit =
  print_string "Printing inferred types of lvalues...";
  print_newline();
  Hashtbl.iter (fun vi -> fun lv ->
		  Printf.printf "%s : %s\n" vi.vname (A.string_of_lvalue lv)
	       ) lvalue_hash 


(** Alias queries. For each function, gather sets of locals, formals, and 
  globals. Do n^2 work for each of these functions, reporting whether or not
  each pair of values is aliased. Aliasing is determined by taking points-to
  set intersections.
*)
let compute_aliases (b : bool) : unit =
  let f_counted = ref 0 in
  let f_total = List.length (!all_functions) in
  let _ = print_string "Computing aliases..."; print_newline() in
  let s_count = ref 0 in
  let a_count = ref 0 in
  let a_total = ref 0 in
  let alias_query (vl : varinfo list) = 
    begin
      let (naive,smart) = A.alias_query b (List.map analyze_var_decl vl)
      in
      incr f_counted;
      a_count := (!a_count) + naive;
      s_count := (!s_count) + smart;
      a_total := (!a_total) + ( (List.length vl) * (List.length vl) )
    end
  in 
  let a_analyze_fundec (f : fundec) = 
    if (!show_progress) then begin
      Printf.printf "Finished %d of %d functions" (!f_counted) f_total;
      print_newline();
      Printf.printf "Scanning function %s" f.svar.vname;
      print_newline()
    end;
     (* todo alias_query ((!all_globals) @ f.sformals @ f.slocals) *)
    alias_query (!all_globals)
  in
    List.iter a_analyze_fundec (!all_functions);
    Printf.printf "Naive queries : %d of %d possible\n" (!a_count) (!a_total);
    Printf.printf "Smart queries : %d of %d possible\n" (!s_count) (!a_total)

let compute_alias_frequency () : unit = 
  let f_counted = ref 0 in
  let f_total = List.length (!all_functions) in
  let _ = print_string "Computing alias frequency..."; print_newline() in
  let s_count = ref 0 in
  let a_count = ref 0 in
  let a_total = ref 0 in
  let alias_frequency (am : access_map) =
    begin
      let accesses = H.fold (fun _ -> fun acc -> fun res -> 
			       acc :: res
			    ) am []
      in
      let (naive,smart) = (A.alias_frequency accesses)
      in
	incr f_counted ;
	a_count := (!a_count) + naive;
	s_count := (!s_count) + smart;
	a_total := (!a_total) + 
	( (List.length accesses) * (List.length accesses) )
    end
  in
  let af_analyze_fundec (f : fundec) = 
    if (!show_progress) then begin
      Printf.printf "Finished %d of %d functions" (!f_counted) f_total;
      print_newline();
      Printf.printf "Scanning function %s" f.svar.vname;
      print_newline()
    end;
    alias_frequency (H.find fun_access_map f)
  in
    List.iter af_analyze_fundec (!all_functions);
    Printf.printf "Naive queries : %d of %d possible\n" (!a_count) (!a_total);
    Printf.printf "Smart queries : %d of %d possible\n" (!s_count) (!a_total)
    


(** abstract location interface *)

type absloc = A.absloc

let rec lvalueVarinfo (vi : varinfo) : A.lvalue =
  try
    H.find lvalue_hash vi
  with 
      Not_found -> failwith ( Pretty.sprint ~width:80 (Pretty.dprintf 
          "lvalueVarinfo unable to find varinfo for %s in lvalue_hash"
          vi.vname ))
and lvalueLval (lv : lval) : A.lvalue =
  match lv with
    | (Var vi, _) -> lvalueVarinfo vi
    | (Mem e, _) -> A.deref (A.rvalue (lvalueExp e))

(* do not export - AddrOf only ok if it's dereferenced later *)
and lvalueExp (e : exp) : A.lvalue =  
  match e with 
    | Lval lv -> lvalueLval lv
    | CastE (_,e) -> lvalueExp e
    | BinOp((PlusPI|IndexPI|MinusPI),e,_,_) -> lvalueExp e

    | (AddrOf lv | StartOf lv) -> 
        A.phonyAddrOf (lvalueLval lv)

    | (Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE
           _ | UnOp _ | BinOp _ )-> 
        failwith "lvalueExp: fishy exp in lval"

(** return an abstract location for a varinfo, resp. lval *)
let abslocVarinfo vi = A.abslocLvalue (lvalueVarinfo vi)
let abslocLval lv = A.abslocLvalue (lvalueLval lv)

let abslocEq = A.abslocEq
let d_absloc = A.d_absloc

