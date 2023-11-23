fun: int -> int:
retone x = ((\(int -> int) y -> y) 1);

println (retone 2);