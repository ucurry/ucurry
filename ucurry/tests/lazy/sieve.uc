datatype List = Nil | Cons of (int * List);

fun : (int -> int) -> List -> List :
map f xs = (case xs of 
                Nil => (Nil) 
                | Cons (y, ys) => (Cons ((f y), (map f ys))));

fun : int -> int:
addone x = x + 1;

fun : unit -> List :
from2 u = (Cons (2, (map addone (from2 ()))));

fun : List -> int -> List :
take l i = if i == 0 then (Nil)
                     else (case l of 
                                Nil => (Nil)
                                | Cons (y, ys) => (Cons (y, (take ys (i - 1)))));


fun : List -> int:
printList l = (case l of
                    Nil => 0
                    | Cons (y, ys) => (begin println y, (printList ys)));

fun : (int -> bool) -> List -> List :
filter f xs = (case xs of 
                Nil => (Nil)
                | Cons (y, ys) => if (f y) then (Cons (y, (filter f ys)))
                                           else (filter f ys));

fun : int -> int -> bool:
notDivisible x y = (y % x) != 0; 

fun : List -> List:
sieve l = (case l of 
            Nil => (Nil)
            | Cons (x, xs) => (Cons (x, 
                                    (sieve ((filter (notDivisible x)) xs)))));

(printList (take (sieve (from2 ())) 4));
