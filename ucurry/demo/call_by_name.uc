fun: int -> int -> int:
fst x y = x;

-- note that this function is an infinite loop
fun: int -> int:
f a = (f a);

println (fst 13 (f 2)); -- in a call-by-name langauge, (f 2) is evaluated, thus this will result in an infinite recursion