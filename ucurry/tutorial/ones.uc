datatype IntList = Nil | Cons of (int * IntList);

fun : unit -> IntList:
ones u = (Cons (1, (ones ())));

fun: IntList -> unit:
printList l = (case l of
                    Nil => ()
                  | Cons (y, ys) => (begin println y, (printList ys)));

(printList (ones ()));


fun : IntList -> int -> IntList:
take l i = if i == 0 
           then (Nil)
           else (case l of 
                       Nil => (Nil)
                     | Cons (y, ys) => (Cons (y, (take ys (i - 1)))));

(printList (take (ones ()) 10));
