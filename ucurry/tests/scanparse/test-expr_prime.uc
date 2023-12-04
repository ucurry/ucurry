-- TODO: currently a type error since filter is not implemented
check_type_error
fun: int list -> int list:
sieve l = (case l of
            [] => [int]
          | x :: xs => x :: (sieve (filter (\(int -> bool) y -> y mod x != 0) xs)));
