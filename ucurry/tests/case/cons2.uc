datatype List = Nil | Cons of (int * List);

List empty = (Nil);
List singleton = (Cons (1, (Nil)));

List another = (Cons (1, empty));

let a = (Cons (1, empty))@Cons in (println a.0);
(println
(case empty of 
    Nil => 0
    | Cons (a, b) => a));

(println 
    (case singleton of 
        Nil => 0
        | Cons (a, b) => a));
