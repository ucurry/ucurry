-- def -> fun1
fun: int -> int -> int :
f x y = x + y;

int x = 1;
fun: unit -> int -> int :
addx = (f x);

int -> int addx2 = (f x);
int y2 = (addx2 2);

int y = ((addx ()) 2);

fun : unit -> int -> int -> int :
ff = f;

fun: unit -> int :
idfun u = 1;

(idfun ());