
-- function f is an identity function but prints its argument
fun: int -> int:
f x = (begin (println x), x);

fun: int -> int -> int:
fst x y = x;

-- in strict evaluation, we will need to evaluate (f 1), so
-- the following program will print (f 1)
(println (fst 2 (f 1)));
