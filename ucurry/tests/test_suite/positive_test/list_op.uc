-- `hd` unop

-- hd operation
println (hd [1]);

-- cons operation
int list xs = [1];
int list a = 2 :: xs;
println (hd a);

-- copy list 
fun: int list -> int list :
copy_list xs = xs; 

int list copied = (copy_list [1,2,3]);
println hd copied;
println (hd (tl copied));
println (hd (tl (tl copied)));
println (null? (tl (tl (tl copied))));

-- compare list , since pattern matching does not yet support tuple, can only use if else for now
fun: int list -> int list -> bool:
compare xs ys = if (null? xs) and (null? ys) 
                then true 
                else (if (null? xs) or (null? ys) 
                     then false 
                    else (if (hd xs) == (hd ys) 
                            then (compare (tl xs) (tl ys))
                            else false));
println (compare [1, 2] [1, 2]);
println (compare [1, 2, 3] [1, 2]);
println (compare [0, 1, 2] [2, 1, 0]);