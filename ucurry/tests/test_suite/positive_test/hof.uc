-- `apply` is a dummy higher-order function that 
-- takes in a function and its argument and applies it
fun : (int -> int) -> int -> int :
apply f x = (f x);

fun : int -> int :
addone x = x + 1;

println (apply addone 3);

-- higher order `map` for integer list
fun: (int -> int) -> int list -> int list:
map f xs = if (null? xs) 
           then [] 
           else ((f (hd xs)) :: (map f (tl xs)));


int list ys = (map addone [1, 2, 3]);

println (hd ys);
