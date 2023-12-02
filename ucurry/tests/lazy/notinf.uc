int y = ~5;

-- `loop` will be an infinite recursion if z is negative
fun: int -> int:
loop z = if z > 0 then z else (loop z);

-- this function negates y if y > 8, otherwise it will
-- be indentity funciton
fun: int -> int:
f x = if y > 8 then x else ~y;

-- we know that the result of `f` will not rely on the
-- computation of `(loop y)` (and it's computation is
-- indeed suspended)
-- if we unfold the definition of f, it will inspect
-- global variable y, then evaluate to ~y
int test = (f (loop y));
println test;

