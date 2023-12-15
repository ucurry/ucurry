fun : int -> int :
addone x = x + 1;

(int -> int) addOne = \(int->int) x -> x + 1;

fun: (int -> int) -> int list -> int list:
map f xs = if (null? xs)
           then [int] 
           else ((f (hd xs)) :: (map f (tl xs)));

int list ys = (map (\(int -> int) x -> x + 1) [1, 2, 3]);
println (hd ys); -- should print 2

addOne 1;