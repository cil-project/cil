(** Utility functions for Coolaid *)
module E = Errormsg
module H = Hashtbl
module IH = Inthash

(* tail-recursive append: reverses xs twice *)
let list_append (xs: 'a list) (ys: 'a list): 'a list =
  match xs with (* optimize some common cases *)
      [] -> ys
    | [x] -> x::ys
    | _ -> List.rev_append (List.rev xs) ys

let list_array_map f l =
  Array.to_list (Array.map f (Array.of_list l))
 
let rec count_map f l ctr =
  match l with
  | [] -> []
  | [x] -> [f x]
  | [x;y] ->
          (* order matters! *)
          let x' = f x in
          let y' = f y in
          [x'; y']
  | [x;y;z] ->
          let x' = f x in
          let y' = f y in
          let z' = f z in
          [x'; y'; z']
  | x :: y :: z :: w :: tl ->
          let x' = f x in
          let y' = f y in
          let z' = f z in
          let w' = f w in
          x' :: y' :: z' :: w' ::
      (if ctr > 500 then list_array_map f tl
       else count_map f tl (ctr + 1))
 
let list_map f l = count_map f l 0

(* Memoize *)
let memoize (h: ('a, 'b) Hashtbl.t) 
            (arg: 'a) 
            (f: 'a -> 'b) : 'b = 
  try
    Hashtbl.find h arg
  with Not_found -> begin
    let res = f arg in
    Hashtbl.add h arg res;
    res
  end

let valOf : 'a option -> 'a = function
    None -> raise (Failure "Util.valOf")
  | Some x -> x

let list_map_opt f = function
  | None -> None
  | Some l -> Some (list_map f l)

let equals x1 x2 : bool =
  (compare x1 x2) = 0

let make_counter () =
  let c = ref 0 in
  (fun () -> incr c; !c),
  (fun () -> c := 0)

let parse_argv_skip_unknown speclist anon_fun =
  let idx = ref 0 in
  let rec parse () =
    try
      Arg.parse_argv ~current:idx Sys.argv speclist anon_fun ""
    with Arg.Bad _ | Arg.Help _ -> parse ()
  in parse ()
