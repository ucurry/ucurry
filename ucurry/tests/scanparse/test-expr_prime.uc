fun: int list -> int list:
sieve l = (case l of
            Nil => []
          | Cons (x, xs) => x :: (sieve (filter (\(int -> bool) y -> y mod x != 0) xs)));
