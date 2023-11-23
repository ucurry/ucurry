int -> int add = \(int->int) x -> x;
add;


fun: int -> (int -> int):
f x = \(int -> int) y -> y + x;
f;

--int z = ((f 1) 2);
--print z;
