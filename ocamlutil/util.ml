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

let equals x1 x2 : bool =
  (compare x1 x2) = 0
