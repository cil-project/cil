
type 'a entry = 'a * int list 

type term_hash = {
  buckets : mutable 'a entry list;
  ub : int;
  size : int;
  capacity : int;
  inserts : int;
}

let create () : term_hash = ()
 
let find (th : term_hash) (stamps : int list) (elt : 'a) : bool = false

val insert (th : term_hash) (stamps : int list) (elt : 'a) : bool = false
