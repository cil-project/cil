                                        (* A hierarchy of timings *)
type t = { name : string;
           mutable time : float;
           mutable sub  : t list}

                                        (* Create the top level *)
let top = { name = "TOTAL";
            time = 0.0;
            sub  = []; }

                                        (* The stack of current path through 
                                         * the hierarchy. The first is the 
                                         * leaf. *)
let current : t list ref = ref [top]

let reset () = top.sub <- []


let print chn msg = 
  let rec prTree ind node = 
    Printf.fprintf chn "%s%-20s          %6.3f s\n" 
      (String.make ind ' ') node.name node.time  ;
    List.iter (prTree (ind + 2)) node.sub
  in
  Printf.fprintf chn "%s" msg;
  List.iter (prTree 0) top.sub
        
let repeattime limit str f arg = 
                                        (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | _ -> assert false in
    let rec loop = function
        h :: _ when h.name = str -> h
      | _ :: rest -> loop rest
      | [] -> 
          let nw = {name = str; time = 0.0; sub = []} in
          curr.sub <- nw :: curr.sub;
          nw
    in
    loop curr.sub
  in
  let oldcurrent = !current in
  current := stat :: oldcurrent;
  let start = (Unix.times ()).Unix.tms_utime in
  let rec loop count = 
    let res   = f arg in
    let finish   = Unix.times () in
    let diff = finish.Unix.tms_utime -. start in
    if diff < limit then
      loop (count + 1)
    else begin
      stat.time <- stat.time +. (diff /. float(count));
      current := oldcurrent;                (* Pop the current stat *)
      res                                   (* Return the function result *)
    end
  in
  loop 1


let time str f arg = repeattime 0.0 str f arg
  


















