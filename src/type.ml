(*
 * C Structural Type Equality
 *)
open Cil
open Pretty

(* Idealized representation of C types to make structural equality
 * comparisons easier. *)
type layout =     
    Scalar of int   (* length in bytes *)
  | Array of (layout list) * int
  | Union of ((layout list) list) * int (* total union size in bytes *)
  | Pointer of typ  (* target is a C/CIL type *)

(* Pretty printing *)
let rec d_layout () l = match l with
    Scalar(i) -> dprintf "Scalar(%d)" i 
  | Array(ll,i) -> dprintf "Array %d %a" i d_layout_list ll
  | Union(lll,i) -> dprintf "Union @[(%a)@]"
      (docList (chr ',' ++ break) (d_layout_list ())) lll
  | Pointer(tau) -> dprintf "Ptr %a" d_type tau
and d_layout_list () ll = 
  dprintf "@[[%a]@]" (docList (chr ';' ++ break) (d_layout ())) ll
    
(* Convert a C/CIL type into a list of layouts. This usually amounts
 * to flattening structures and making padding and alignment explicit. *)
let rec convert_type_to_layout_list (t : typ) =
  let t = unrollType t in 
  match t with
    TVoid _ -> failwith "convert_type_to_layout_list: void"
  | TInt _ | TFloat _ | TEnum _ -> [Scalar((bitsSizeOf t) / 8)]
  | TPtr(tau,_) -> [Pointer(tau)]
  | TArray(tau,Some(e),_) -> 
      let ll = convert_type_to_layout_list tau in
      let len = begin match isInteger e with
        Some(i64) -> Int64.to_int i64
      | None -> failwith "convert_type_to_layout_list: non-const length"
      end in [Array(ll,len)]
  | TArray(tau,None,_) -> 
      failwith "convert_type_to_layout_list: empty array"
  | TComp(ci,al) when ci.cstruct ->
      let total_size = (bitsSizeOf t) / 8 in
      let seen_size = ref 0 in 
      let ll = ref [] in 
      List.iter (fun fi -> 
        let field_size_bits = bitsSizeOf fi.ftype in 
        let field_offset_bits, field_width_bits = 
          bitsOffset t (Field(fi,NoOffset)) in
        let field_offset = field_offset_bits / 8 in 
        let field_size = field_size_bits / 8 in 
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
      let total_size = (bitsSizeOf t) / 8 in
      let lll = List.map (fun fi ->
        let this_size = (bitsSizeOf fi.ftype) / 8 in
        let ll = convert_type_to_layout_list fi.ftype in
        if this_size < total_size then
          ll @ [Scalar(total_size - this_size)]
        else
          ll 
      ) ci.cfields in
      [Union(lll,total_size)]
  | TComp _ -> failwith "convert_type_to_layout_list: mystery comp"
  | TFun _ -> failwith "convert_type_to_layout_list: function pointer"
  | TNamed _ -> failwith "convert_type_to_layout_list: named"
  
(* Strips 'i' bytes of scalars from the beginning of the layout list 'll'.
 * Returns a new layout lest representing the suffix or raises an 
 * exception on failure. *)
let rec strip_scalar_prefix (ll : layout list) i =
  if i = 0 then 
    ll
  else if i < 0 then 
    Scalar(-i) :: ll 
  else match ll with
    [] -> failwith "out of layout"
  | Scalar(j) :: tl -> strip_scalar_prefix tl (i - j)
  | Array(inner_ll,0) :: tl -> strip_scalar_prefix tl i
  | Array(inner_ll,1) :: tl -> strip_scalar_prefix (inner_ll @ tl) i
  | Array(inner_ll,j) :: tl -> 
      let new_ll = inner_ll @ (Array(inner_ll,j-1) :: tl) in
      strip_scalar_prefix new_ll i 
  | Union(lll,ts) :: (tl : layout list) ->
      if ts > i then begin (* strip just part of the union *)
        let lll = List.map (fun ll -> strip_scalar_prefix ll i) lll in
        Union(lll,(ts - i)) :: tl 
      end else begin (* strip the whole union away! *)
        List.iter (fun ll -> ignore (strip_scalar_prefix ll ts)) lll ;
        strip_scalar_prefix tl (i - ts)
      end
  | Pointer _ :: tl -> failwith "strip scalar from pointer?" 

(* Physical/Structural Equivalence Classes of type *)
module OrderedType = struct type t = typ let compare = compare end
module TypeSet = Set.Make(OrderedType)
module TypeUF = Unionfind.Make(TypeSet)

(* a global set of type equivalence classes *)
let global_eq = ref (TypeUF.empty)
let global_subtype = Hashtbl.create 511

(* Check to see if two types are physically/structurally equal. This
 * is the function that other modules should call to check equality. *)
let rec equal t1 t2 =
  let s1 = bitsSizeOf t1 in
  let s2 = bitsSizeOf t2 in
  if s1 <> s2 then
    false
  else if TypeUF.check_equal !global_eq t1 t2 then
    true
  else begin
    try 
      let answer, new_eq_classes = equal_types t1 t2 !global_eq in
      if answer then begin
        global_eq := new_eq_classes
      end ;
      answer
    with _ -> 
      false 
  end
(* Is small a physical/structural subtype of big? Only the top-level layout
 * can actually be a subtype. Must be invariant under pointers. *)
and subtype small big = 
  let s1 = bitsSizeOf small in
  let s2 = bitsSizeOf big in
  if s1 > s2 then
    false
  else if Hashtbl.mem global_subtype (small,big) then 
    true
  else begin
    try 
      let l1 = convert_type_to_layout_list small in
      let l2 = convert_type_to_layout_list big in
      let answer, new_eq_classes = equal_ll l1 l2 !global_eq true in 
      if answer then begin
        global_eq := new_eq_classes ;
        Hashtbl.add global_subtype (small,big) true
      end ;
      answer
    with _ -> 
      false 
  end
and equal_types tau1 tau2 (assumed_eq_classes : TypeUF.t) =
  let l1 = convert_type_to_layout_list tau1 in
  let l2 = convert_type_to_layout_list tau2 in
  let new_eq_classes = TypeUF.make_equal assumed_eq_classes tau1 tau2 in
  equal_ll l1 l2 (new_eq_classes : TypeUF.t) false
and equal_ll l1 l2 (aec : TypeUF.t) subtyping =
  match l1, l2 with
    [], [] -> (true,aec)
  | [], _ when subtyping -> (true, aec)
  | (Scalar(i) :: tl), _ -> 
      let new_lhs = tl in
      let new_rhs = strip_scalar_prefix l2 i in
      equal_ll new_lhs new_rhs aec subtyping
  | (Array(inner_ll1,1) :: tl1) , _ ->
      let new_lhs = inner_ll1 @ tl1 in
      equal_ll new_lhs l2 aec subtyping
  | (Array(inner_ll1,i1) :: tl1) , _ ->
      let new_lhs = inner_ll1 @ (Array(inner_ll1,i1-1) :: tl1) in
      equal_ll new_lhs l2 aec subtyping
  | (Union(lll,ts) :: tl1) , _ ->
      let assumes = ref aec in
      let union_answer = List.fold_left (fun acc elt -> 
        if not acc then false else begin
          let answer, nec = equal_ll (elt @ tl1) l2 !assumes subtyping in
          assumes := nec ;
          answer
        end) true lll in
      union_answer, !assumes
  | (Pointer(tau1) :: tl1) , (Pointer(tau2) :: tl2) -> 
      if TypeUF.check_equal aec tau1 tau2 then 
        equal_ll tl1 tl2 aec subtyping
      else begin
        let answer, nec = equal_types tau1 tau2 aec in
        if answer then 
          equal_ll tl1 tl2 nec subtyping
        else
          (false, aec)
      end
  | _ -> (false, aec)

(* Debugging Information *)
let global_eq_classes () = 
  TypeUF.eq_classes !global_eq
