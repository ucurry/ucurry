datatype Num = Zero | Addone of Num;

fun : unit -> Num : 
ones u = (Addone (ones ()));

fun : Num -> int -> int : 
take n i = if i == 0 
           then 100 
           else (case n of Zero => 0 
                | Addone (x) => 1 + (take x (i - 1)));

println (take (ones ()) 1);
