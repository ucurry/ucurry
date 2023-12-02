fun : int -> int :
fact n = if n == 1 then 1 else  (fact (n - 1)) * n ; 
println (fact 3);
--println (fact 1);
--(begin println "this", 1);