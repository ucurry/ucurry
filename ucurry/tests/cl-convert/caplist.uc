int list x = [1, 2, 3];
fun : int -> int list : 
f a = x ;

println hd (f 1);

fun : (int -> int list) -> int -> int list:
app f a = (f a); 

println hd (app f 1);
