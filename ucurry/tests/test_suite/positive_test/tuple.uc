-- define times for a tuple
fun: (int * int) -> int:
times t = t.0 * t.1; 

println (times (2, 3));