-- (lambda (f) (lambda (x) (lambda (y) (f x y))))

\((int -> int -> int) -> int) f -> \(int -> (int -> int)) x -> \(int -> int)y -> (f x y); 
