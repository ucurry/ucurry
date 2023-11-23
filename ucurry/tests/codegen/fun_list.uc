fun: int list -> int : 
car xs = hd xs;
println (car [1, 2]);

fun: int list -> int list:
cdr xs = tl xs;
println (car (cdr [1, 2, 3]));

