(* cilint: infinite-precision, 2's complement arithmetic. *)

(* An infinite-precision 2's complement number is a good way of
   understanding the meaning of bitwise operations on infinite
   precision numbers: positive numbers have the normal base-2
   representation, while negative numbers are represented in
   a 2's complement representation with an infinite series of
   leading 1 bits. I.e.,

     3 = ...0000000000011
    -3 = ...1111111111101

   We represent cilints using a big_int, except that we specialise the
   case where the number fits in a regular int. This specialisation
   has two benefits:
   - more compact (and potentially faster ops, though more would need to be
   specialised for this to be really worth it)
   - ability to see the value of small constants in ocamldebug

   The implementation can be simplified once OCaml 3.11.1 shows up, with
   bitwise operations on big_ints, and bug-fixed versions of int64_of_big_int
   and big_int_of_int64. *)

type cilint = Small of int | Big of Z.t
type truncation = NoTruncation | ValueTruncation | BitTruncation

let zero_cilint : cilint = Small 0
let one_cilint : cilint = Small 1

(* Precompute useful big_ints *)
let b30 = Z.pow (Z.of_int 2) 30
let m1 = Z.minus_one
let two = Z.of_int 2

(* True if 'b' is all 0's or all 1's *)
let nobits (b:Z.t) : bool = 
  Z.sign b = 0 || Z.equal b m1

let big_int_of_cilint (c:cilint) : Z.t = 
  match c with
  | Small i -> Z.of_int i
  | Big b -> b


let cilint_of_big_int (b:Z.t) : cilint =
  if Z.fits_int b then
    Small (Z.to_int b)
  else
    Big b 


let neg_cilint (c : cilint) : cilint = 
  match c with
  | Small((i: int)) 
     when i <> Pervasives.min_int -> 
      Small(-i)
  | _ -> Big (Z.neg (big_int_of_cilint c))

(* Apply big_int 'op' to two cilints, returning a cilint *)
let b op c1 c2 = cilint_of_big_int (op (big_int_of_cilint c1) (big_int_of_cilint c2))
  
let add_cilint = b Z.add
let sub_cilint = b Z.sub
let mul_cilint = b Z.mul
let div_cilint = b Z.div
let mod_cilint = b Z.rem 


let compare_cilint (c1:cilint) (c2:cilint) : int = 
  match c1, c2 with
  | Small i1, Small i2 -> Pervasives.compare i1 i2
  | _ -> Z.compare (big_int_of_cilint c1) (big_int_of_cilint c2)

let is_zero_cilint (c:cilint) : bool = 
  match c with
  | Small i -> i = 0
  | Big b -> Z.sign b = 0

let negative_cilint (c:cilint) : bool = 
  match c with 
  | Small i -> i < 0
  | Big b -> Z.sign b < 0

let cilint_of_int (i:int) : cilint = Small i

let int_of_cilint (c:cilint) : int = 
  match c with 
  | Small i -> i
  | Big b -> Z.to_int b

let rec cilint_of_int64 (i64:int64) : cilint = 
  if Int64.compare i64 (Int64.of_int min_int) >= 0 && 
    Int64.compare i64 (Int64.of_int max_int) <= 0 then
    Small (Int64.to_int i64)
  else
    (* We convert 30 bits at a time *)
    let rec loop i mul acc = 
      if i = 0L then acc
      else if i = -1L then Z.sub acc mul
      else 
	let lo30 = Int64.to_int (Int64.logand i 0x3fffffffL) in
	loop (Int64.shift_right i 30) (Z.mul mul b30)
	  (Z.add acc (Z.mul mul (Z.of_int lo30)))
    in Big (loop i64 Z.one Z.zero)
   
(* Note that this never fails, instead it returns the low-order 64-bits
   of the cilint. *)
let rec int64_of_cilint (c:cilint) : int64 = 
  match c with
  | Small i -> Int64.of_int i
  | Big b -> 
      let rec loop b mul acc = 
	if Z.sign b = 0 then 
	  acc
	else if Z.compare b m1 == 0 then
	  Int64.sub acc mul
	else
	  let hi, lo = Z.ediv_rem b b30 in
	  loop hi (Int64.mul mul 0x40000000L)
	    (Int64.add acc (Int64.mul mul (Int64.of_int (Z.to_int lo))))
      in loop b 1L 0L
      
let cilint_of_string (s:string) : cilint = 
  cilint_of_big_int (Z.of_string s)

let string_of_cilint (c:cilint) : string = 
  match c with 
  | Small i -> string_of_int i
  | Big b -> Z.to_string b

(* Divide rounding towards zero *)
let div0_cilint (c1:cilint) (c2:cilint) = 
  match c1, c2 with
  | Small i1, Small i2 -> Small (i1 / i2)
  | _ ->
      let b1 = big_int_of_cilint c1 in
      let b2 = big_int_of_cilint c2 in 
      let q, r = Z.ediv_rem b1 b2 in
      if Z.lt b1 Z.zero && (not (Z.equal r Z.zero)) then
	if Z.gt b2 Z.zero then
	  Big (Z.succ q)
	else
	  Big (Z.pred q)
      else
	Big q

(* And the corresponding remainder *)
let rem_cilint (c1:cilint) (c2:cilint) = 
  (sub_cilint c1 (mul_cilint c2 (div0_cilint c1 c2)))

(* Perform logical op 'op' over 'int' on two cilints. Does it work
   30-bits at a time as that is guaranteed to fit in an 'int'. *)
let logop op c1 c2 = 
  match c1, c2 with
  | Small i1, Small i2 -> Small (op i1 i2)
  | _ ->
      let b1 = big_int_of_cilint c1 in
      let b2 = big_int_of_cilint c2 in 
      let rec loop b1 b2 mul acc =
	if nobits b1 && nobits b2 then
	  (* Once we only have all-0/all-1 values left, we can find whether
	     the infinite high-order bits are all-0 or all-1 by checking the
	     behaviour of op on b1 and b2. *)
	  if op (Z.to_int b1) (Z.to_int b2) = 0 then
	    acc
	  else
	    Z.sub acc mul
	else
	  let hi1, lo1 = Z.ediv_rem b1 b30 in
	  let hi2, lo2 = Z.ediv_rem b2 b30 in
	  let lo = op (Z.to_int lo1) (Z.to_int lo2) in
	  loop hi1 hi2 (Z.mul mul b30) 
	    (Z.add acc (Z.mul mul (Z.of_int lo)))
      in cilint_of_big_int (loop b1 b2 Z.one Z.zero)

let logand_cilint = logop (land)
let logor_cilint = logop (lor)
let logxor_cilint = logop (lxor)

let shift_right_cilint (c1:cilint) (n:int) : cilint = 
  match c1 with
  | Small i -> Small (i asr n)
  | Big b -> cilint_of_big_int (Z.shift_right b n)

let shift_left_cilint (c1:cilint) (n:int) : cilint = 
  cilint_of_big_int (Z.shift_left (big_int_of_cilint c1) n)

let lognot_cilint (c1:cilint) : cilint = 
  match c1 with
  | Small i -> Small (lnot i)
  | Big b -> Big (Z.pred (Z.neg b))

let truncate_signed_cilint (c:cilint) (n:int) : cilint * truncation =
  match c with
  | Small i when n >= Nativeint.size - 1 -> Small i, NoTruncation
  | Small i when n < Nativeint.size - 2 -> 
      let max = 1 lsl (n - 1) in
      let truncmax = 1 lsl n in
     let bits = i land (truncmax - 1) in
      let tval = 
    (* check if the n-th bit is 1... *)
	if bits < max then
	  bits
	else
    (* and fill with 1 bits on the left if it is *)
	  bits - truncmax
      in
      let trunc = 
	if i >= max || i < -max then
	  if i >= truncmax then
	    BitTruncation
	  else
	    ValueTruncation
	else
	  NoTruncation
      in Small tval, trunc
  | _ ->
      let b = big_int_of_cilint c in
      let max = Z.pow two (n - 1) in
      let truncmax = Z.pow two n in
      let bits = Z.rem b truncmax in
      let tval = 
	if Z.lt bits max then
	  bits
	else
	  Z.sub bits truncmax
      in
      let trunc = 
	if Z.geq b max || Z.lt b (Z.neg max) then
	  if Z.geq b truncmax then 
	    BitTruncation
	  else
	    ValueTruncation
	else
	  NoTruncation
      in cilint_of_big_int tval, trunc

let truncate_unsigned_cilint (c:cilint) (n:int) : cilint * truncation =
  match c with
  | Small i when i > 0 && n >= Nativeint.size - 2 -> Small i, NoTruncation
  | Small i when n < Nativeint.size - 2 ->
      let max = 1 lsl (n - 1) in
      let truncmax = 1 lsl n in
      let bits = i land (truncmax - 1) in
      let trunc = 
	if i >= truncmax || i < 0 then
	  if i < -max then
	    BitTruncation
	  else
	    ValueTruncation
	else
	  NoTruncation
      in Small bits, trunc
  | _ ->
      let b = big_int_of_cilint c in
      let max = Z.pow two (n - 1) in
      let truncmax = Z.pow two n in
      let bits = Z.rem b truncmax in
      let trunc = 
	if Z.geq b truncmax || Z.lt b Z.zero then
	  if Z.lt b (Z.neg max) then
	    BitTruncation
	  else
	    ValueTruncation
	else
	  NoTruncation
      in cilint_of_big_int bits, trunc
	
let is_int_cilint (c:cilint) : bool = 
  match c with
  | Small _ -> true
  | Big b -> Z.fits_int b
