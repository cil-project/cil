(** Utilities for managing "concatenable lists" (clists). We often need to
    concatenate sequences, and using lists for this purpose is expensive. This
    module provides routines to manage such lists more efficiently. In this
    model, we never do cons or append explicitly. Instead we maintain
    the elements of the list in a special data structure. Routines are provided
    to convert to/from ordinary lists, and carry out common list operations.*)

(** The clist datatype. A clist can be an ordinary list, or a clist preceded 
    or followed by an element, or two clists implicitly appended together*)
type 'a clist = 
  | CList of 'a list             (** The only representation for the empty 
                                     list *)
  | CConsL of 'a * 'a clist  
  | CConsR of 'a clist * 'a 
  | CSeq of 'a clist * 'a clist (** We concatenate only two of them at this
                                    time. Neither is the empty clist. To be
                                    sure always use append to make these *)


(** Convert a clist to an ordinary list *)
val toList: 'a clist -> 'a list

(** Convert an ordinary list to a clist *)  
val fromList: 'a list -> 'a clist 

(** Create a clist containing one element *)
val single: 'a -> 'a clist        

(** The empty clist *)
val empty: 'a clist               

(*
val fromList: 'a list -> 'a clist
val fromListRev: 'a list -> 'a clist  (* reverse the list *)
*)

(** Append two clists *)
val append: 'a clist -> 'a clist -> 'a clist 

(*
(** Splits into the head and the tail. The head is None iff the original
   list is empty *)
val hdtl: 'a clist -> 'a option * 'a clist 
*)

(** Find the length of a clist *)
val length: 'a clist -> int   

(** Map a function over a clist. Returns another clist *)
val map: ('a -> 'b) -> 'a clist -> 'b clist 

(*
(** Replaces each element with an entire list *)
val mapList: ('a -> 'b clist) -> 'a clist -> 'b clist 

*)

(** A version of fold_left that works on clists *)
val fold_left: ('acc -> 'a -> 'acc) -> 'acc -> 'a clist -> 'acc

(** A version of iter that works on clists *)
val iter: ('a -> unit) -> 'a clist -> unit

(** Reverse a clist *)
val rev: 'a clist -> 'a clist

(** A document for printing a clist (similar to [docList]) *)
val docCList: 
    Pretty.doc -> ('a -> Pretty.doc) -> unit -> 'a clist -> Pretty.doc


