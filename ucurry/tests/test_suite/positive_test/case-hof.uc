fun : int -> int -> int:
notDivisible x y = (y % x);

fun : (int -> int) -> int -> int:
fakeFilter f x = (f 1);

fun : int -> int :
fakeSieve i = (fakeFilter (notDivisible 1) 2);

println (fakeSieve 2);