exception GotSignal of int

val withTimeout : float -> (* Seconds for timeout *)
                (int -> 'b) -> (* What to do if we have a timeout. The 
                                        * argument passed is the signal number 
                                        * received. *)
                ('a -> 'b) -> (* The function to run *)
                'a -> (* And its argument *)
   'b

val docHash : ('a -> 'b -> Pretty.doc) -> unit -> 
  (('a, 'b) Hashtbl.t) -> Pretty.doc 


val hash_to_list: ('a, 'b) Hashtbl.t -> ('a * 'b) list

val keys: ('a, 'b) Hashtbl.t -> 'a list


(** Copy a hash table into another *)
val hash_copy_into: ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit

(** First, a few utility functions I wish were in the standard prelude *)

val anticompare: 'a -> 'a -> int

val list_drop : int -> 'a list -> 'a list
val list_span: ('a -> bool) -> ('a list) -> 'a list * 'a list
val list_insert_by: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list
val list_head_default: 'a -> 'a list -> 'a
val list_iter3 : ('a -> 'b -> 'c -> unit) ->
  'a list -> 'b list -> 'c list -> unit
val get_some_option_list : 'a option list -> 'a list

(** Iterate over a list passing the index as you go *)
val list_iteri: (int -> 'a -> unit) -> 'a list -> unit
val list_mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

(** Like fold_left but pass the index into the list as well *)
val list_fold_lefti: ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc

val int_range_list : int -> int -> int list

(* Create a list of length l *)
val list_init : int -> (int -> 'a) -> 'a list

(** Growable arrays *)
type 'a growArrayFill =
    Elem of 'a
  | Susp of (int -> 'a)

type 'a growArray = {
            gaFill: 'a growArrayFill;
            (** Stuff to use to fill in the array as it grows *)
    mutable gaData: 'a array;
  } 

val newGrowArray: int -> 'a growArrayFill -> 'a growArray
val getReg: 'a growArray -> int -> 'a
val setReg: 'a growArray -> int -> 'a -> unit
val copyGrowArray: 'a growArray -> 'a growArray


(** hasPrefix prefix str returns true with str starts with prefix *)
val hasPrefix: string -> string -> bool


(** Given a ref cell, produce a thunk that later restores it to its current value *)
val restoreRef: 'a ref -> unit -> unit

(** Given a hash table, produce a thunk that later restores it to its current value *)
val restoreHash: ('a, 'b) Hashtbl.t -> unit -> unit

(** Given an array, produce a thunk that later restores it to its current value *)
val restoreArray: 'a array -> unit -> unit


(** Given a list of thunks, produce a thunk that runs them all *)
val runThunks: (unit -> unit) list -> unit -> unit
