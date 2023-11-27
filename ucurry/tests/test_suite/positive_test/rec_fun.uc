fun : int -> int :
fact n = if n == 1 then 1 else n * (fact (n - 1)); 
println (fact 5);

fun : int -> int :
fib n = if n <= 1 then 1 else (fib (n - 1)) + (fib (n - 2));
println (fib 9);
