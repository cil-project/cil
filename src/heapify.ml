(* 
 * Heapify: a program transform that looks over functions, finds those
 * that have local (stack) variables that contain arrays, puts all such
 * local variables into a single heap allocated structure, changes all
 * accesses to such variables into accesses to fields of that structure
 * and frees the structure on return. 
 *)
open Cil

(* utilities that should be in Cil.ml *)
let mkSimpleField ci fn ft =
  { fcomp = ci ; fname = fn ; ftype = ft ; fbitfield = None ; fattr = []; }

(* actual Heapify begins *)
class containsArray result = object (* does this type contain an array? *)
  inherit nopCilVisitor  (* only visit types *)
  method vtype t = match t with
    TArray _ -> result := true ; SkipChildren (* found an array *)
  | _ -> DoChildren
end

class heapifyModifyVisitor big_struct big_struct_fields varlist free = object
  inherit nopCilVisitor  (* visit lvalues and statements *)
  method vlval l = match l with (* should we change this one? *)
    Var(vi),vi_offset when List.mem_assoc vi varlist -> (* check list *)
      let i = List.assoc vi varlist in (* find field offset *)
      let big_struct_field = List.nth big_struct_fields i in
      let new_lval = Mem(Lval(big_struct, NoOffset)),
				Field(big_struct_field,vi_offset) in (* rewrite the lvalue *)
      ChangeTo(new_lval)
  | _ -> DoChildren (* ignore other lvalues *)
  method vstmt s = match s.skind with (* also rewrite the return *)
    Return(_,loc) -> let free_stmt = mkStmt 
				(Instr [Call(None,free,[Lval(big_struct,NoOffset)],loc)]) in
			let new_block = mkBlock [free_stmt ; s] in (* call free, then return *)
			ChangeTo(mkStmt (Block(new_block)))  
	| _ -> DoChildren (* ignore other statements *)
end

class heapifyAnalyzeVisitor f alloc free = object 
  inherit nopCilVisitor (* only look at function bodies *)
  method vglob gl = match gl with
		GFun(fundec,funloc) -> 
    let counter = ref 0 in (* the number of local vars containing arrays *)
    let varlist = ref [] in  (* a list of (var,id) pairs *)
    List.iter (fun vi ->  (* find all local vars with arrays *)
			let result = ref false in 
      visitCilType (new containsArray result) vi.vtype ;
      if !result then begin (* this local var contains an array *)
				varlist := (vi,!counter) :: !varlist ; (* add it to the list *)
				incr counter (* put the next such var in the next slot *)
      end ;
    ) fundec.slocals ; 
    if (!varlist <> []) then begin (* some local vars contain arrays *)
      let name = (fundec.svar.vname ^ "_heapify") in
      let ci = mkCompInfo true name (* make a big structure *)
				(fun _ -> List.map (* each local var becomes a field *)
					(fun (vi,i) -> vi.vname,vi.vtype,None,[]) !varlist) [] in
      let vi = makeLocalVar fundec name (TPtr(TComp(ci,[]),[])) in
      let modify = new heapifyModifyVisitor (Var(vi)) 
				ci.cfields !varlist free in (* rewrite accesses to local vars *)
      fundec.sbody <- visitCilBlock modify fundec.sbody ;
      let alloc_stmt = mkStmt (* allocate the big struct on the heap *)
      (Instr [Call(Some(Var(vi),NoOffset), alloc, 
				[SizeOf(TComp(ci,[]))],locUnknown)]) in
      fundec.sbody.bstmts <- alloc_stmt :: fundec.sbody.bstmts ; 
			fundec.slocals <- List.filter (fun vi -> (* remove local vars *)
				not (List.mem_assoc vi !varlist)) fundec.slocals ; 
			let typedec = (GCompTag(ci,locUnknown)) in (* declare the big struct *)
      ChangeTo([typedec ; GFun(fundec,funloc)])  (* done! *)
    end else
			DoChildren	(* ignore everything else *)
	| _ -> DoChildren
end

let heapify (f : file) (alloc : exp) (free : exp)  =
  visitCilFile (new heapifyAnalyzeVisitor f alloc free) f
(* heapify code ends here *)

let default_heapify (f : file) =
	let alloc_fun = emptyFunction "malloc" in
	let free_fun = emptyFunction "free" in
	let alloc_exp = (Lval((Var(alloc_fun.svar)),NoOffset)) in
	let free_exp = (Lval((Var(free_fun.svar)),NoOffset)) in
	ignore (heapify f alloc_exp free_exp)

(* StackGuard clone *)

class sgModifyVisitor restore_ra_stmt = object
	inherit nopCilVisitor
  method vstmt s = match s.skind with (* also rewrite the return *)
    Return(_,loc) -> let new_block = mkBlock [restore_ra_stmt ; s] in 
			ChangeTo(mkStmt (Block(new_block)))  
	| _ -> DoChildren (* ignore other statements *)
end

class sgAnalyzeVisitor f push pop get_ra set_ra = object
	inherit nopCilVisitor
	method vfunc fundec = 
		let needs_guarding = List.fold_left (fun acc vi ->
			acc || let result = ref false in
			visitCilType (new containsArray result) vi.vtype ;
			!result) false fundec.slocals in
		if needs_guarding then begin
			let ra_tmp = makeLocalVar fundec "return_address" voidPtrType in
			let ra_exp = Lval(Var(ra_tmp),NoOffset) in 
			let save_ra_stmt = mkStmt (* save the current return address *)
			(Instr [Call(Some(Var(ra_tmp),NoOffset), get_ra, [], locUnknown) ;
						  Call(None, push, [ra_exp], locUnknown)]) in
			let restore_ra_stmt = mkStmt (* restore the old return address *)
			(Instr [Call(Some(Var(ra_tmp),NoOffset), pop, [], locUnknown) ;
						  Call(None, set_ra, [ra_exp], locUnknown)]) in
      let modify = new sgModifyVisitor restore_ra_stmt in
      fundec.sbody <- visitCilBlock modify fundec.sbody ;
			fundec.sbody.bstmts <- save_ra_stmt :: fundec.sbody.bstmts ;
      ChangeTo(fundec)  (* done! *)
		end else DoChildren
end

let stackguard (f : file) (push : exp) (pop : exp) 
	(get_ra : exp) (set_ra : exp) = 
	visitCilFile (new sgAnalyzeVisitor f push pop get_ra set_ra) f
(* stackguard code ends *)

let default_stackguard (f : file) =
	let expify fundec = Lval(Var(fundec.svar),NoOffset) in
	let push = expify (emptyFunction "stackguard_push") in
	let pop = expify (emptyFunction "stackguard_pop") in
	let get_ra = expify (emptyFunction "stackguard_get_ra") in
	let set_ra = expify (emptyFunction "stackguard_set_ra") in
	let global_decl = 
"struct stackguard_stack {
	void * data;
	struct stackguard_stack * next;
 } * stackguard_stack;
 void stackguard_push(void *ra) {
	void * old = stackguard_stack;
	stackguard_stack = malloc(sizeof(stackguard_stack));
	stackguard_stack->data = ra;
	stackguard_stack->next = old;
 }
 void * stackguard_pop() {
	void * ret = stackguard_stack->data;
	void * next = stackguard_stack->next;
	free(stackguard_stack);
	stackguard_stack->next = next;
	return ret;
 }" in
	f.globals <- GText(global_decl) :: f.globals ;
	ignore (stackguard f push pop get_ra set_ra )
