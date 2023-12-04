-- functions are curried, we can do partial application
fun: int -> int -> int :
f x y = x + y;

fun: int -> int :
addone = (f 1);

(println (addone 2));
(println (f 1 2));
(println ((f 1) 2));
