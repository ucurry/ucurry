datatype Intlist = Nil | Cons of int * Intlist;


int x = 1;
fun: List -> int:
f a = (case a of 
            Nil => x
            | Cons (_, ys) => 1 + (f ys));

