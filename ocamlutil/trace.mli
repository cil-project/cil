(* Trace module
 * Scott McPeak, 5/4/00
 *
 * The idea is to pepper the source with debugging printfs,
 * and be able to select which ones to actually display at
 * runtime.
 *
 * It is built on top of the Pretty module for printing data
 * structures.
 *
 * To a first approximation, this is needed to compensate for
 * the lack of a debugger that does what I want...
 *)


(* this is the list of tags (usually subsystem names) for which
 * trace output will appear *)
val traceSubsystems : string list ref

(* interface to add a new subsystem to trace (slightly more
 * convenient than direclty changing 'tracingSubsystems') *)
val traceAddSys : string -> unit

(* query whether a particular subsystem is being traced *)
val traceActive : string -> bool
   
(* add several systems, separated by commas *)
val traceAddMulti : string -> unit


(* current indentation level for tracing *)
val traceIndentLevel : int ref

(* bump up or down the indentation level, if the given subsys
 * is being traced *)
val traceIndent : string -> unit
val traceOutdent : string -> unit


(* this is the trace function; its first argument is a string
 * tag, and second argument is a 'doc' (which is what 'dprintf'
 * returns).
 *
 * so a sample usage might be
 *   (trace "mysubsys" (dprintf "something neat happened %d times" counter))
 *)
val trace : string -> Pretty.doc -> unit


(* special flavors that indent/outdent as well.  the indent version
 * indents *after* printing, while the outdent version outdents
 * *before* printing.  thus, a sequence like
 *
 *   (tracei "foo" (dprintf "beginning razzle-dazzle\n"))
 *     ..razzle..
 *     ..dazzle..
 *   (traceu "foo" (dprintf "done with razzle-dazzle\n"))
 *
 * will do the right thing
 *
 * update -- I changed my mind!  I decided I prefer it like this
 *   %%% sys: (myfunc args)
 *     %%% ...inner stuff...
 *     %%% sys: myfunc returning 56
 *
 * so now they both print before in/outdenting
 *)
val tracei : string -> Pretty.doc -> unit
val traceu : string -> Pretty.doc -> unit
