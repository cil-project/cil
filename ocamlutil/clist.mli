(* We often need to concatenate sequences and using lists for this purpose is 
 * expensive. So we define a kind of "concatenable lists" that are easier to 
 * concatenate *)
type 'a clist = 
  | CList of 'a list              (* The only representation for the empty 
                                   * list  *)
  | CConsL of 'a * 'a clist
  | CConsR of 'a clist * 'a 
  | CSeq  of 'a clist * 'a clist (* We concatenate only two of them at this 
                                  * time. Neither is CEmpty. To be sure 
                                  * always use append to make these  *)

val toList: 'a clist -> 'a list 
val fromList: 'a list -> 'a clist


val single: 'a -> 'a clist
val empty: 'a clist

(*
val fromList: 'a list -> 'a clist
val fromListRev: 'a list -> 'a clist  (* reverse the list *)
*)

val append: 'a clist -> 'a clist -> 'a clist

(*
val hdtl: 'a clist -> 'a option * 'a clist (* Splits into the head and the 
                                            * tail. The head is None iff the 
                                            * original list is empty  *)
*)
val length: 'a clist -> int


val map: ('a -> 'b) -> 'a clist -> 'b clist

(*
val mapList: ('a -> 'b clist) -> 'a clist -> 'b clist (* Replaces each 
                                                       * element with an 
                                                       * entire list  *)

*)
val fold_left: ('acc -> 'a -> 'acc) -> 'acc -> 'a clist -> 'acc
val iter: ('a -> unit) -> 'a clist -> unit

val rev: 'a clist -> 'a clist

val docCList: 
    Pretty.doc -> ('a -> Pretty.doc) -> unit -> 'a clist -> Pretty.doc

