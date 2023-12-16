fun: (int -> int -> int) -> int -> int list -> int:
foldl f acc xs =
  (case xs of
    [] => acc
  | (y :: ys) => (foldl f (f acc y) ys));

fun: (int -> int -> int) -> int -> int list -> int:
foldr f acc xs =
  (case xs of
    [] => acc
  | (y :: ys) => (f y (foldr f acc ys)));

fun: int -> int -> int:
add x y = x + y;

-- same as sum
println (foldl add 0 [1, 2, 3, 4, 5]);
println (foldr add 0 [1, 2, 3, 4, 5]);

fun: int -> int -> int:
sub x y = x - y;
println (foldl sub 0 [1, 2, 3, 4, 5]);
println (foldr sub 0 [1, 2, 3, 4, 5]);

