
                                        (* Resets all the timings *)
val reset : unit -> unit

                                        (* Time a function and associate the 
                                         * time with the given string. If 
                                         * some timing information is already 
                                         * associated with that string, then 
                                         * accumulate the times *)
                                        
                                        (* If this function is invoked 
                                         * within another timed function then 
                                         * you can have a hierarchy of 
                                         * timings *)
val time  : string -> ('a -> 'b) -> 'a -> 'b


                                        (* repeattime is like time but runs 
                                         * the function several times until 
                                         * the total running time is greater 
                                         * or equal to the first argument. 
                                         * The total time is then divided by 
                                         * the number of times the function 
                                         * was run. *)
val repeattime : float -> string -> ('a -> 'b) -> 'a -> 'b

                                        (* Print the current stats preceeded 
                                         * by a message *)
val print : out_channel -> string -> unit
