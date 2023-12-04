-- Anonymous function 
println ((\(int -> int) x -> x + 1) 1);

println ((\(int -> int -> int) x y -> x + y) 10 20);

(int -> int) addone = \(int->int) x -> x + 1;
println (addone 1);