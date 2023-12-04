fun: unit -> int :
idfun u = 1;
println (idfun ());

fun : unit -> (unit -> int):
f u = \(unit -> int) u -> 1;
((f ()) ());

-- automatic curry
fun: int -> int -> int :
g x y = x + y;

fun: int -> int:
addone = (g 1);
