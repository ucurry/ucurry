datatype List = Nil | Cons of (int * List);

fun : unit -> List:
ones u = (Cons (1, (ones ())));

fun : List -> int -> int:
take l i = if i == 0 
           then 0 
           else (case l of 
                    Nil => 0
                    | Cons (a, b) => (begin (println a), (take b (i - 1))));

(take (ones ()) 15);
