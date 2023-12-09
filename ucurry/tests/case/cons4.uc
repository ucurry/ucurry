datatype List = Nil | Cons of (int * List);

fun : (int -> int) -> List -> List :
map f xs = (case xs of 
                Nil => (Nil) 
                | Cons (y, ys) => (Cons ((f y), (map f ys))));
