type 'a term_hash
val create : unit -> term_hash
val find : term_hash -> int list -> 'a
val insert : term_hash -> int list -> 'a -> bool
