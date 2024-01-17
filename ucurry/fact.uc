fun: int -> int:
fact n = if n == 0 then 1 else n * (fact (n - 1));
println (fact 10);