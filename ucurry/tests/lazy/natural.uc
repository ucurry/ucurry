datatype List = Nil | Cons of (int * List);

fun : (int -> int) -> List -> List :
map f xs = (case xs of 
                Nil => (Nil) 
                | Cons (y, ys) => (Cons ((f y), (map f ys))));

fun : int -> int:
addone x = x + 1;

fun : unit -> List :
nat u = (begin println "nat", (Cons (1, (map addone (nat ())))));

--fun : List -> int -> List :
--take l i = if i == 0 then (Nil)
                     --else (case l of 
                                --Nil => (Nil)
                                --| Cons (y, ys) => (Cons (y, (take ys (i - 1)))));

fun : List -> int -> int :
takePrint l i = 
    if i == 0 then 0
    else  (case l of 
            Cons (y, ys) => (begin println y, (takePrint ys (i - 1)))
            | Nil => 0);
(takePrint (nat ()) 2);

--fun : List -> int:
--printList l = (case l of
                    --Nil => 0
                    --| Cons (y, ys) => (begin println y, (printList ys)));

(printList (take (nat ()) 11));