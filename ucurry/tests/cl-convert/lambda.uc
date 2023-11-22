fun: int -> (int -> int):
f x = \(int -> int) y -> y + x;

int z = ((f 1) 2);
--print z;
