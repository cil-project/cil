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


(** First, a few utility functions I wish were in the standard prelude *)

val anticompare: 'a -> 'a -> int
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

val list_init : int -> (int -> 'a) -> 'a list

(** Growable arrays *)
type 'a growArray = {
            gaFill: 'a; (** Stuff to use to fill in the array as it grows *)
    mutable gaData: 'a array;
  } 
val newGrowArray: int -> 'a -> 'a growArray
val getReg: 'a growArray -> int -> 'a
val setReg: 'a growArray -> int -> 'a -> unit
val copyGrowArray: 'a growArray -> 'a growArray

