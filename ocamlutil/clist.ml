open Pretty

(* We often need to concatenate sequences and using lists for this purpose is 
 * expensive. So we define a kind of "concatenable lists" that are easier to 
 * concatenate *)
type 'a clist = 
  | CList of 'a list             (* This is the only representation for empty 
                                  * *)
  | CConsL of 'a * 'a clist
  | CConsR of 'a clist * 'a 
  | CSeq  of 'a clist * 'a clist (* We concatenate only two of them at this 
                                  * time. Neither is CEmpty. To be sure 
                                  * always use append to make these  *)

let rec listifyOnto (tail: 'a list) = function
    CList l -> l @ tail
  | CConsL (x, l) -> x :: listifyOnto tail l
  | CConsR (l, x) -> listifyOnto (x :: tail) l
  | CSeq (l1, l2) -> listifyOnto (listifyOnto tail l2) l1
        
let toList l = listifyOnto [] l
let fromList l = CList l
    
    
let single x = CList [x]
let empty = CList []
    
let append l1 l2 = 
  if l1 == CList [] then l2 else
  if l2 == CList [] then l1 else
  CSeq (l1, l2)
    
let rec length (acc: int) = function
    CList l -> acc + (List.length l)
  | CConsL (x, l) -> length (acc + 1) l
  | CConsR (l, _) -> length (acc + 1) l
  | CSeq (l1, l2) -> length (length acc l1) l2
let length l = length 0 l  (* The external version *)
    
let map (f: 'a -> 'b) (l: 'a clist) : 'b clist = 
  let rec loop = function
      CList l -> CList (List.map f l)
    | CConsL (x, l) -> let x' = f x in CConsL (x', loop l)
    | CConsR (l, x) -> let l' = loop l in CConsR (l', f x)
    | CSeq (l1, l2) -> let l1' = loop l1 in CSeq (l1', loop l2)
  in
  loop l
    
(*
let mapList (f: 'a -> 'b clist) (l: 'a clist) : 'b clist = 
  let rec loop = function
      CList l -> 
    | CConsL (x, l) -> let x' = f x in append x' (loop l)
    | CConsR (l, x) -> let l' = loop l in append l' (f x)
    | CSeq (l1, l2) -> let l1' = loop l1 in append l1' (loop l2)
  in
  loop l
  *)
  
let fold_left (f: 'acc -> 'a -> 'acc) (start: 'acc) (l: 'a clist) = 
  let rec loop (start: 'acc) = function
      CList l -> List.fold_left f start l
    | CConsL (x, l) -> loop (f start x) l
    | CConsR (l, x) -> let res = loop start l in f res x
    | CSeq (l1, l2) -> 
        let res1 = loop start l1 in
        loop res1 l2
  in
  loop start l
    
let iter (f: 'a -> unit) (l: 'a clist) : unit = 
  let rec loop = function
      CList l -> List.iter f l
    | CConsL (x, l) -> f x; loop l
    | CConsR (l, x) -> loop l; f x
    | CSeq (l1, l2) -> loop l1; loop l2
  in
  loop l
    
(*
let rec hdtl (l: 'a clist) : 'a option * 'a clist = 
  match l with
    CEmpty -> None, CEmpty
  | CConsL (x, l) -> Some x, l
  | CConsR (CEmpty, x) -> Some x, CEmpty
  | CConsR (l, x) -> 
      let (h, t) = hdtl l in
      (h, CConsR (t, x))
  | CSeq (l1, l2) -> (* We know that l1 is not empty *)
      assert (l1 != CEmpty);
      let (h, t) = hdtl l1 in
      (h, CSeq (t, l2))
*)
        
let rec rev = function
    CList l -> CList (List.rev l)
  | CConsL (x, l) -> CConsR (rev l, x)
  | CConsR (l, x) -> CConsL (x, rev l)
  | CSeq (l1, l2) -> CSeq (rev l2, rev l1)

(*        
let rec fromList = function
    [] -> CEmpty
  | x :: rest -> CConsL(x, fromList rest)
        
let rec fromListRevOnto (tail: 'a clist) = function
    [] -> tail
  | x :: rest -> fromListRevOnto (CConsL(x, tail)) rest
let fromListRev l = fromListRevOnto CEmpty l
*)

let docCList (sep: doc) (doone: 'a -> doc) () (dl: 'a clist) = 
  fold_left 
    (fun (acc: doc) (elem: 'a) -> 
      let elemd = doone elem in
      if acc == nil then elemd else acc ++ sep ++ elemd)
    nil
    dl
