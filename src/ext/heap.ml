(* The type of a heap (priority queue): keys are integers, data values
 * are whatever you like *)
type ('a) t = {
          elements  : (int * ('a option)) array ;
  mutable size      : int ; (* current number of elements *)
          capacity  : int ; (* max number of elements *)
} 

let create size = {
  elements = Array.create (size+1) (max_int,None) ;
  size = 0 ;
  capacity = size ; 
} 

let clear heap = heap.size <- 0  

let is_full heap = (heap.size = heap.capacity) 

let is_empty heap = (heap.size = 0) 

let insert heap prio elt = begin
  if is_full heap then begin
    raise (Invalid_argument "Heap.insert")
  end ; 
  heap.size <- heap.size + 1 ;
  let i = ref heap.size in
  while ( fst heap.elements.(!i / 2) < prio ) do
    heap.elements.(!i) <- heap.elements.(!i / 2) ;
    i := (!i / 2)
  done ;
  heap.elements.(!i) <- (prio,Some(elt))
  end

let examine_max heap = 
  if is_empty heap then begin
    raise (Invalid_argument "Heap.examine_max")
  end ; 
  match heap.elements.(1) with
    p,Some(elt) -> p,elt
  | p,None -> failwith "Heap.examine_max" 

let extract_max heap = begin
  if is_empty heap then begin
    raise (Invalid_argument "Heap.extract_max")
  end ; 
  let max = heap.elements.(1) in
  let last = heap.elements.(heap.size) in
  heap.size <- heap.size - 1 ; 
  let i = ref 1 in
  let break = ref false in 
  while (!i * 2 <= heap.size) && not !break do
    let child = ref (!i * 2) in

    (* find smaller child *)
    if (!child <> heap.size && 
        fst heap.elements.(!child+1) > fst heap.elements.(!child)) then begin
        incr child 
    end ; 

    (* percolate one level *) 
    if (fst last < fst heap.elements.(!child)) then begin
      heap.elements.(!i) <- heap.elements.(!child) ; 
      i := !child 
    end else begin
      break := true 
    end
  done ; 
  heap.elements.(!i) <- last ;
  match max with 
    p,Some(elt) -> p,elt
  | p,None -> failwith "Heap.examine_min" 
  end


