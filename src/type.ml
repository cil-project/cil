(*
 * C Structural Type Equality
 *)
open Cil
open Pretty
module E = Errormsg

(* Idealized representation of C types to make structural equality
 * comparisons easier. *)
type layout =     
    Scalar of int   (* length in bytes *)
  | Array of (layout list) * int
  | Union of ((layout list) list) * int (* total union size in bytes *)
  | Pointer of typ (* original CIL pointer type
                    * either TPtr or TFun *)
  | Anything of int (* length in bytes, matches anything *)

let bytesSizeOf tau = 
  try
    bitsSizeOf(tau) / 8
  with _ -> 1

(* Pretty printing *)
let rec d_layout () l = match l with
    Scalar(i) -> dprintf "Scalar(%d)" i 
  | Anything(i) -> dprintf "Anything(%d)" i 
  | Array(ll,i) -> dprintf "Array %d %a" i d_layout_list ll
  | Union(lll,i) -> dprintf "Union @[(%a)@]"
      (docList (chr ',' ++ break) (d_layout_list ())) lll
  | Pointer(tau) -> dprintf "Ptr (%a)" d_type tau
and d_layout_list () ll = 
  dprintf "@[[%a]@]" (docList (chr ';' ++ break) (d_layout ())) ll
    
(* Convert a C/CIL type into a list of layouts. This usually amounts
 * to flattening structures and making padding and alignment explicit. *)
let rec convert_type_to_layout_list (t : typ) =
  let t = unrollType t in 
  match t with
    TVoid _ -> (* failwith "convert_type_to_layout_list: void" *) [] 
  | TInt _ | TFloat _ | TEnum _ -> [Scalar((bytesSizeOf t))]
  | TPtr(_) -> [Pointer(t)]
  | TFun(_) -> [Pointer(t)]
  | TArray(tau,Some(e),_) when e = one -> convert_type_to_layout_list tau 
  | TArray(tau,Some(e),_) -> 
      let ll = convert_type_to_layout_list tau in
      let e = constFold true e in 
      let len = begin match isInteger e with
        Some(i64) -> Int64.to_int i64
      | None -> 
          E.s (E.unimp 
            "type: convert_type_to_layout_list: non-const length %a" 
            d_type t)
      end in [Array(ll,len)]
  | TArray(tau,None,_) -> 
      E.s (E.unimp "type: convert_type_to_layout_list: empty array %a"
            d_type t)
  | TComp(ci,al) when ci.cstruct ->
      let total_size = (bytesSizeOf t) in
      let seen_size = ref 0 in 
      let ll = ref [] in 
      List.iter (fun fi -> 
        let field_size_bytes = bytesSizeOf fi.ftype in 
        let field_offset_bits, field_width_bits = 
          bitsOffset t (Field(fi,NoOffset)) in
        let field_offset = field_offset_bits / 8 in 
        let field_size = field_size_bytes in 
        if (!seen_size < field_offset) then begin
          ll := !ll @ [Scalar(field_offset - !seen_size)];
          seen_size := field_offset 
        end ;
        let field_ll = convert_type_to_layout_list fi.ftype in
        ll := !ll @ field_ll ;
        seen_size := !seen_size + field_size ; 
      ) ci.cfields ;
      if (!seen_size < total_size) then begin
        ll := !ll @ [Scalar(total_size - !seen_size)];
      end ;
      !ll
  | TComp(ci,al) when not ci.cstruct -> 
      let total_size = (bytesSizeOf t) in
      let lll = List.map (fun fi ->
        let this_size = (bytesSizeOf fi.ftype) in
        let ll = convert_type_to_layout_list fi.ftype in
        if this_size < total_size then
          ll @ [Anything(total_size - this_size)]
        else
          ll 
      ) ci.cfields in
      [Union(lll,total_size)]
  | TComp _ -> E.s (E.bug "type: convert_type_to_layout_list: mystery comp")
  | TNamed _ -> E.s (E.bug "type: convert_type_to_layout_list: named")
  
(* Strips 'i' bytes of scalars from the beginning of the layout list 'll'.
 * Returns a boolean success code and new layout lest representing the
 * suffix or raises an exception on failure. *)
let rec strip_scalar_prefix (ll : layout list) i =
  if i = 0 then 
    true, ll
  else if i < 0 then 
    true, (Scalar(-i) :: ll)
  else match ll with
    [] -> false, [] 
  | Scalar(j) :: tl -> (strip_scalar_prefix tl (i - j))
  | Anything(j) :: tl -> 
      if (i >= j) then (* eat up the entire "Anything" *)
        (strip_scalar_prefix tl (i - j))
      else
        true, (Anything(j - i) :: tl)
  | Array(inner_ll,0) :: tl -> (strip_scalar_prefix tl i)
  | Array(inner_ll,1) :: tl -> (strip_scalar_prefix (inner_ll @ tl) i)
  | Array(inner_ll,j) :: tl -> 
      let new_ll = inner_ll @ (Array(inner_ll,j-1) :: tl) in
      (strip_scalar_prefix new_ll i )
  | Union(lll,ts) :: (tl : layout list) ->
      if ts > i then begin (* strip just part of the union *)
        let final_answer = ref true in 
        let lll = List.map (fun ll -> 
          let answer, new_ll = strip_scalar_prefix ll i in
          if answer = false then final_answer := false ;
          new_ll) lll in
        !final_answer, (Union(lll,(ts - i)) :: tl )
      end else begin (* strip the whole union away! *)
        let final_answer = ref true in 
        List.iter (fun ll -> let answer, _ = (strip_scalar_prefix ll ts) in
          if answer = false then final_answer := false
          ) lll ;
        let answer, result = strip_scalar_prefix tl (i - ts) in
        (!final_answer && answer),result
      end
  | Pointer(_) :: tl -> 
      let _, res = strip_scalar_prefix (Scalar(4) :: tl) i in
      false, res

(* Physical/Structural Equivalence Classes of type *)
module OrderedType = struct type t = typ let compare = compare end
module TypeSet = Set.Make(OrderedType)
module TypeUF = Unionfind.Make(TypeSet)
(* INVARIANT: always put unrolled types in here *)

(* a global set of type equivalence classes *)
let global_eq = ref (TypeUF.empty)
let global_subtype = Hashtbl.create 511

(* When we are computing "t1 < t2" or "t1 > t2", which one can be
 * smaller? *)
type subtype_direction = LeftCanEndEarly | RightCanEndEarly | MustBeEqual

let flip_subtyping st = match st with
    LeftCanEndEarly -> RightCanEndEarly 
  | RightCanEndEarly -> LeftCanEndEarly 
  | MustBeEqual -> MustBeEqual 

(* Check to see if two types are physically/structurally equal. This
 * is the function that other modules should call to check equality. 
 * fun_to_apply is called on all types that are checked for equality
 * as a result 
 *
 * failure : type -> type -> unit
 *   is called on types enclosing equality failiures
 * compat : type -> type -> unit
 *   is called on types that must be equal for these types to be equal
 * *)
let rec equal ?(compat=(fun _ _ -> ())) ?(failure=(fun _ _ -> ())) t1 t2 =
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in
  let s1 = bytesSizeOf t1 in
  let s2 = bytesSizeOf t2 in
  if s1 <> s2 then
    false
  else if TypeUF.check_equal !global_eq t1 t2 then
    true
  else begin
    try 
      let answer, new_eq_classes = 
        equal_types t1 t2 !global_eq compat failure t1 t2
      in
      if answer then begin
        global_eq := new_eq_classes
      end ;
      answer || (t1 = t2) (* fall back on ML equality *)
    with _ -> 
      (t1 = t2)  (* fall back on ML equality *) 
  end
(* Is small a physical/structural subtype of big? Only the top-level layout
 * can actually be a subtype. Must be invariant under pointers. *)
and subtype ?(compat=(fun _ _ -> ())) ?(failure=(fun _ _ -> ())) small big =
  let small = unrollType small in
  let big = unrollType big in
  let s1 = bytesSizeOf small in
  let s2 = bytesSizeOf big in
  if s1 > s2 then begin
    (* Pretty.printf "subtype: %a %a failed because %d > %d\n"
      d_type small d_type big s1 s2 ;  *)
    false
  end else if Hashtbl.mem global_subtype (small,big) then 
    true
  else begin
    try 
      let l1 = convert_type_to_layout_list small in
      let l2 = convert_type_to_layout_list big in
      let answer, new_eq_classes = 
        equal_ll l1 l2 !global_eq LeftCanEndEarly compat failure big small in 
      if answer then begin
        global_eq := new_eq_classes ;
        Hashtbl.add global_subtype (small,big) true
      end else begin
      (*
        Pretty.printf "subtype: %a %a failed because@!--> %a@!--> %a@!"
          d_type small d_type big 
          d_layout_list l1 d_layout_list l2 ; 
          *)
        () 
      end ; 
      answer || (big = small) (* fall back on ML equality *) 
    with e -> 
      (* Pretty.printf "subtype: %a %a failed because %s\n"
        d_type small d_type big (Printexc.to_string e) ;  *)
      (big = small) (* fall back on ML equality *) 
  end
and equal_types tau1 tau2 (assumed_eq_classes : TypeUF.t) compat failure f1 f2=
  let tau1 = unrollType tau1 in
  let tau2 = unrollType tau2 in
  let l1 = convert_type_to_layout_list tau1 in
  let l2 = convert_type_to_layout_list tau2 in
  let new_eq_classes = TypeUF.make_equal assumed_eq_classes tau1 tau2 in
  equal_ll l1 l2 (new_eq_classes : TypeUF.t) MustBeEqual compat failure f1 f2 
(* 
 * arguments: 
 *   l1 l2     // check and see if these two layout lists are equal
 *   aec       // assumed equivalence classes 
 *   subtyping // if this is true, l1 can be shorter than l2
 *   compat    // call this function on all lined-up pointers in l1, l2
 *   failure f1 f2 // call 'failure f1 f2' if they are not equal
 *)
and equal_ll l1 l2 (aec : TypeUF.t) subtyping compat failure f1 f2 =
  let final_answer, aec = 
  match l1, l2 with
    [], [] -> (true,aec)
  | [], _ when subtyping = LeftCanEndEarly -> (true, aec)
  | _, [] when subtyping = RightCanEndEarly -> (true, aec)
  | [], _ 
  | _, [] -> failure f1 f2 ; (false, aec)

  | (Scalar(i) :: tl), _ -> 
      let new_lhs = tl in
      let worked, new_rhs = strip_scalar_prefix l2 i in
      if worked = false then failure f1 f2 ;
      let answer, nec = 
        equal_ll new_lhs new_rhs aec subtyping compat failure f1 f2 in
      (answer && worked), nec

  | (Anything(i) :: tl), _ -> 
      let new_lhs = tl in
      let _, new_rhs = strip_scalar_prefix l2 i in
      equal_ll new_lhs new_rhs aec subtyping compat failure f1 f2 

  (* array on left *)
  | (Array(inner_ll1,i1) :: tl1) , _ ->
      let new_lhs = 
        if i1 = 0 then
          tl1 
        else if i1 = 1 then 
          inner_ll1 @ tl1 
        else 
          inner_ll1 @ (Array(inner_ll1,i1-1) :: tl1)
      in 
      equal_ll new_lhs l2 aec subtyping compat failure f1 f2 

  (* union on left *)
  | (Union(lll,ts) :: tl1) , _ -> 
      let assumes = ref aec in
      let union_answer = List.fold_left (fun acc elt -> 
        if not acc then false else begin
          let answer, nec = 
            equal_ll (elt @ tl1) l2 !assumes subtyping compat failure f1 f2 in
          assumes := nec ;
          answer
        end) true lll in
      union_answer, !assumes

  (* array/union/anything on right *)
  | _, (Array(_) :: _)
  | _, (Union(_) :: _) 
  | _, (Anything(_) :: _) -> 
      (* flip things around, use the code above *)
      equal_ll l2 l1 aec (flip_subtyping subtyping) compat failure f1 f2

  | (Pointer(tau1) :: tl1) , (Pointer(tau2) :: tl2) -> 
      (* already unrolled *)
      if tau1 = tau2 || TypeUF.check_equal aec tau1 tau2 then begin
        compat tau1 tau2 ; 
        equal_ll tl1 tl2 aec subtyping compat failure f1 f2 
      end else begin
        (* first, check under the pointers *)
        match tau1, tau2 with

        | TPtr(TFun(t1,vlo1,_,_),_), 
          TPtr(TFun(t2,vlo2,_,_),_)  (* two fun ptrs *)
        | TFun(t1,vlo1,_,_), TFun(t2,vlo2,_,_) -> (* two fun ptrs *)
            let aec = ref aec in 
            let a1, nec1 = equal_types t1 t2 !aec compat failure tau1 tau2 in
            aec := nec1 ; 
            let al1 = argsToList vlo1 in
            let al2 = argsToList vlo2 in
            let a2 = 
              if List.length al1 <> List.length al2 then begin (* failure *)
                failure tau1 tau2 ;
                false
              end else begin
                (* check every argument *)
                let answer = ref true in
                List.iter2 (fun arg1 arg2 ->
                  let ans, nec2 = equal_types arg1.vtype arg2.vtype 
                    !aec compat failure tau1 tau2 in
                  aec := nec2 ;
                  answer := ans && !answer ;
                ) al1 al2 ; 
                !answer
              end
            in (a1 && a2),!aec

        | TFun _ , _  (* function pointer and non-function ptr *)
        | _, TFun _ -> begin
                    failure f1 f2 ;
                    let _, nec = (* check the rest anyway *)
                      equal_ll tl1 tl2 aec subtyping compat failure f1 f2 in
                    (false, nec)
                    end

        | TPtr(inner1,_), TPtr(inner2,_) -> begin (* two non-fun pointers *)
                    compat tau1 tau2 ; 
                    let answer, nec = equal_types tau1 tau2 
                      aec compat failure inner1 inner2 in
                    let answer', nec' = (* check the rest anyway *)
                      equal_ll tl1 tl2 nec subtyping compat failure f1 f2 in
                    ((answer' && answer),nec')
                  end

        | _, _ -> E.s (E.bug "type: unexpected pointers %a and %a"
          d_type tau1 d_type tau2) 
      end

  | (_ :: tl1), (_ :: tl2) -> (* NOT EQUAL *)
      (* but check the rest anyway! *)
      failure f1 f2 ; 
      let answer, new_aec = 
        equal_ll tl1 tl2 aec subtyping compat failure f1 f2 in
      (false, new_aec) 
  in
  (* ignore (E.warn "subtype: %b@!%a@!%a"
    final_answer d_layout_list l1 d_layout_list l2) ;*)
  (final_answer, aec)

(* Debugging Information *)
let global_eq_classes () = 
  TypeUF.eq_classes !global_eq
