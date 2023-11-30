int y = 0;

fun: int -> int:
loop z = if z > 0 then z else (loop z);

-- fun: int -> int:
-- f x = if y > 8 then x else ~y;

-- (f (loop y));

