fun : int -> int :
g x = letrec f = \(int -> int) n ->  
    if n == 1 
    then 1
    else n * (f (n - 1)) in (f x); 
println (g 3);
