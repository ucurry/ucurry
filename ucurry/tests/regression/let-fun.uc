fun : int -> int :
g x = let f = \(int -> int) n ->  
    if n == 1 
    then 1
    else n * (f (n - 1)) in (f x); 
(g 3);