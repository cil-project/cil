exception Bad_find

type 'a urefC =
    Ecr of 'a * int
  | Link of 'a uref
and 'a uref = 'a urefC ref

let rec find p = 
  match !p with
    | Ecr _ -> p
    | Link p' ->
	let p'' = find p' 
	in p := Link p''; p''

let uref x = ref (Ecr(x,0))

let equal (p,p') = (find p == find p')

let deref p = 
  match ! (find p) with 
    | Ecr (x,_) -> x
    | _ -> raise Bad_find

let update (p,x) = 
  let p' = find p 
  in
    match !p' with
      | Ecr (_,rank) -> p' := Ecr(x,rank)
      | _ -> raise Bad_find
	  
let unify f (p,q) = 
  let p',q' = find p, find q in
    match (!p',!q') with 
      | (Ecr(px,pr),Ecr(qx,qr)) ->
	let x = f(px,qx) in
	  if (p' == q') then
	    p' := Ecr(x,pr)
	  else if pr == qr then
	    (q' := Ecr(x,qr+1); p' := Link q')
	  else if pr < qr then
	    (q' := Ecr(x,qr); p' := Link q')
	  else (* pr > qr *)
	    (p' := Ecr(x,pr); q' := Link p')
      | _ -> raise Bad_find
	  
let union (p,q) = 
  let p',q' = find p, find q in
    match (!p',!q') with 
      | (Ecr(px,pr),Ecr(qx,qr)) ->
	  if (p' == q') then 
	    ()
	  else if pr == qr then
	    (q' := Ecr(qx, qr+1); p' := Link q')
	  else if pr < qr then
	    p' := Link q'
	  else (* pr > qr *)
	    q' := Link p'
      | _ -> raise Bad_find
	  

