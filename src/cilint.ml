(* cilint: infinite-precision, 2's complement arithmetic. *)

(* An infinite-precision 2's complement number is a good way of
   understanding the meaning of bitwise operations on infinite
   precision numbers: positive numbers have the normal base-2
   representation, while negative numbers are represented in
   a 2's complement representation with an infinite series of
   leading 1 bits. I.e.,

     3 = ...0000000000011
    -3 = ...1111111111101

   We represent cilints using a big_int but define some operations differently *)

open Big_int_Z

type cilint = big_int
type truncation = NoTruncation | ValueTruncation | BitTruncation

let zero_cilint = zero_big_int
let one_cilint = unit_big_int
let mone_cilint = minus_big_int unit_big_int

(* Precompute useful big_ints *)
let b30 = shift_left_big_int unit_big_int 30
let m1 = minus_big_int unit_big_int

(* True if 'b' is all 0's or all 1's *)
let nobits (b:big_int) : bool =
  sign_big_int b = 0 || compare_big_int b m1 = 0

let big_int_of_cilint (c:cilint) : big_int = c
let cilint_of_big_int (b:big_int) : cilint = b

let neg_cilint = minus_big_int

let add_cilint = add_big_int
let sub_cilint = sub_big_int
let mul_cilint = mult_big_int
let div_cilint = div_big_int
let mod_cilint = mod_big_int

let compare_cilint = compare_big_int
let is_zero_cilint (c:cilint) : bool = sign_big_int c = 0

let cilint_of_int = big_int_of_int
let int_of_cilint = int_of_big_int

let cilint_of_int64 (i64:int64) : cilint = big_int_of_int64 i64

(* Note that this never fails, instead it returns the low-order 64-bits
   of the cilint. *)
let int64_of_cilint (b:cilint) : int64 =
  let rec loop b mul acc =
    if sign_big_int b = 0 then
      acc
    else if compare_big_int b m1 == 0 then
      Int64.sub acc mul
    else
      let hi, lo = quomod_big_int b b30 in
      loop hi (Int64.mul mul 0x40000000L)
        (Int64.add acc (Int64.mul mul (Int64.of_int (int_of_big_int lo))))
  in loop b 1L 0L

let cilint_of_string = big_int_of_string
let string_of_cilint = string_of_big_int

(* Divide rounding towards zero *)
let div0_cilint (c1:cilint) (c2:cilint) =
  let q, r = quomod_big_int c1 c2 in
  if lt_big_int c1 zero_big_int && (not (eq_big_int r zero_big_int)) then
    if gt_big_int c2 zero_big_int then
      (succ_big_int q)
    else
      (pred_big_int q)
  else
    q

(* And the corresponding remainder! Different from  *)
(* Big_int_Z.mod_big_int computes the Euclidian Modulus, but what we want here is the remainder, as returned by mod on ints
    -1 rem 5 == -1, whereas -1 Euclid-Mod 5 == 4
*)
let rem_cilint (c1:cilint) (c2:cilint) =
  (sub_cilint c1 (mul_cilint c2 (div0_cilint c1 c2)))

(* Perform logical op 'op' over 'int' on two cilints. Does it work
   30-bits at a time as that is guaranteed to fit in an 'int'. *)
let logop op c1 c2 =
  let rec loop b1 b2 mul acc =
    if nobits b1 && nobits b2 then
      (* Once we only have all-0/all-1 values left, we can find whether
        the infinite high-order bits are all-0 or all-1 by checking the
        behaviour of op on b1 and b2. *)
      if op (int_of_big_int b1) (int_of_big_int b2) = 0 then
        acc
      else
        sub_big_int acc mul
    else
      let hi1, lo1 = quomod_big_int b1 b30 in
      let hi2, lo2 = quomod_big_int b2 b30 in
      let lo = op (int_of_big_int lo1) (int_of_big_int lo2) in
      loop hi1 hi2 (mult_big_int mul b30)
        (add_big_int acc (mult_big_int mul (big_int_of_int lo)))
  in loop c1 c2 unit_big_int zero_big_int

let logand_cilint = logop (land)
let logor_cilint = logop (lor)
let logxor_cilint = logop (lxor)

let shift_right_cilint (c1:cilint) (n:int) : cilint =
  shift_right_towards_zero_big_int c1 n

let shift_left_cilint (c1:cilint) (n:int) : cilint =
  shift_left_big_int c1 n

let lognot_cilint (c1:cilint) : cilint = (pred_big_int (minus_big_int c1))

let truncate_signed_cilint (c:cilint) (n:int) : cilint * truncation =
  let max = shift_left_big_int unit_big_int (n - 1) in
  let truncmax = shift_left_big_int unit_big_int n in
  let bits = mod_big_int c truncmax in
  let tval = if lt_big_int bits max then
	    bits
	  else
	    sub_big_int bits truncmax
  in
  let trunc =
    if ge_big_int c max || lt_big_int c (minus_big_int max) then
      if ge_big_int c truncmax then
        BitTruncation
      else
        ValueTruncation
    else
      NoTruncation
  in
    tval, trunc

let truncate_unsigned_cilint (c:cilint) (n:int) : cilint * truncation =
  let max = shift_left_big_int unit_big_int (n - 1) in
  let truncmax = shift_left_big_int unit_big_int n in
  let bits = mod_big_int c truncmax in
  let trunc =
    if ge_big_int c truncmax || lt_big_int c zero_big_int then
      if lt_big_int c (minus_big_int max) then
        BitTruncation
      else
        ValueTruncation
	  else
	    NoTruncation
  in
  bits, trunc

let is_int_cilint (c:cilint) : bool = is_int_big_int c
