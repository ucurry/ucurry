(* renaming for let blocks *)
module C = Cast

(* the reasong is that we want to directly translate local
   names to local registers in LLVM
   but we cannot do
   x = 1;
   let y = (let x = 2 in 2) in x + y
   
   cannot be directory translated to

   x = 1;
   y = (x = 2);
   x + y;
*)

(* now it's just identity *)
let rename: C.program -> C.program = function 
  p -> p

