fun : (int -> bool) -> int list -> int list: 
filter f nums = (case nums of [] => [int]
                  | (h :: t) => if (f h) 
                              then (h :: (filter f t))
                              else (filter f t));
fun : int -> bool :
even num = (num % 2 == 0);
println (even 2);
println (even 1);

int list evens = (filter even [1, 2, 3, 4]);
println hd evens;
println (hd (tl evens));
println (null? (tl (tl evens)));