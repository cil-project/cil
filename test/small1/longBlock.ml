open Cil

;;

initCIL ();

let variable = makeGlobalVar "value" intType in

let instructions = ref [] in
for loop = 1 to 25000 do
  let instruction = Set(var variable, integer loop, locUnknown) in
  instructions := instruction :: !instructions
done;

let statement = mkStmt (Instr !instructions) in
dumpStmt defaultCilPrinter stdout 0 statement
