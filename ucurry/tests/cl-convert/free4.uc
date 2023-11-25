-- (lambda (f) (lambda (x) (lambda (y) ( x + y + z))))

int a = ((((\ (int -> int -> int -> int) x -> 
                \ (int -> int -> int) y   -> 
                \ (int -> int) z -> x + y + z) 1 ) 2) 3);
println a;
