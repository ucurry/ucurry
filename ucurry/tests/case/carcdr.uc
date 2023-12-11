datatype List = Nil | Cons of (int * List);

List empty = (Nil);
List singleton = (Cons (1, (Nil)));
List ones = (Cons (1, (Cons (1, (Cons (1, (Nil)))))));

fun: List -> int : 
car xs = (case xs of 
            Cons (x, _) => x
            | Nil => 0
        );

(car singleton);

fun: List -> List :
cdr xs = (case xs of 
            Cons (_, xs) => xs
            | Nil => empty
);

(cdr singleton);

fun: List -> int:
caar xs = (car (cdr xs));

(caar ones);

--random pattern matching
println (case ones of 
    Cons (a, Cons (b, Nil)) => b
    | Cons (a, Nil) => a
    | Cons (a, b) => a
    | Nil => 0);



