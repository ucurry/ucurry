(int * int) a = (1, 1);
(int * bool) b = (1+2, true);

fun: int -> int:
f x = x;

((int -> int) * string) c = (f, "hello");


fun: int -> int -> int:
add x y =(begin (println "no way"), x + y);

((int -> int -> int) * bool) d = (add, false);
println (d.1);

(int * bool) e = ((add 1 2), false);
println e.1;


