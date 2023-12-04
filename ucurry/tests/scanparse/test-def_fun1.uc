-- def -> fun1
fun: int -> int:
h x = x;

-- higher-order function
fun: int -> int:
copy = h;

fun: (int -> int) -> int:
g f = (f 1);

(copy 1);
(g h);

fun: int -> int -> int:
mult x = (\(int -> int) y -> 1);
(mult 1);

-- arity > 2
fun: int -> int -> int :
f x y = x + y;
(f 1 2);

-- partial application
int x = 1;
fun: int -> int :
addx = (f x);
(addx 1);

fun :int -> int -> int :
ff = f;

--int y = (addx 2);


-- 0-arg function
fun: unit -> int :
idfun u = 1;

(idfun ());


-- multi-unit function
fun: unit -> unit -> int :
idfun2 u = (\(unit -> int) u -> 1);

(idfun2 ());
-- After PAP is done TODO uncomment
--(units () ());