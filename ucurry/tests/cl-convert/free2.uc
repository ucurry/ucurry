int a = 100;
fun: int -> int :
f x = ((\(int -> int) b -> a) 1);
a = 200;
println (f 2);
