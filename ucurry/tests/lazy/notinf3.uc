fun: int -> int -> int:
fst x y = x;

-- note that this function is an infinite loop
fun: int -> int:
f a = (f a);
-- normally a recursive function should look like this:
-- f a = if a == 0 then 0 else (begin (println a), (f (a - 1)));

-- for example, uncommenting this line should result in an infinite loop
-- (f 2);

println (fst 13 (f 2));
