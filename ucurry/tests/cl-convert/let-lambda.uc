int -> int f = let x = 1 in \(int -> int) y -> x; 
int x = 4;
(println (f 2));

int -> int g = 
    let x = 1 , w = 3 in \(int -> int) y -> 
                        ((\(int -> int) z -> y + z + x + w) x);
(println (g 3));
