datatype Tree = Node of (int * Tree * Tree) | Leaf;

datatype List = Nil | Cons of (int * List);

-- Tree b1 = (Node (4, (Leaf), (Leaf)));

Tree t1 = (Node (3, (Node (1, (Leaf), (Node (2, (Leaf), (Leaf))))), (Node (4, (Leaf), (Leaf)))));

Tree t2 = (
    (Node (5, 
        (Node (3, 
            (Node (2, 
                (Node (1, (Leaf), (Leaf))), 
                (Leaf)
            )), 
            (Node (4, (Leaf), (Leaf)))
        )), 
        (Node (8,
            (Node (7,
                (Node (6, (Leaf), (Leaf))),
                (Leaf)
            )), 
            (Node (10, 
                (Node (9, (Leaf), (Leaf))), 
                (Leaf)
            ))
        ))
    ))
);


fun: List -> List -> List:
append l1 l2 = (case l1 of 
                    Nil => l2
                    | Cons (x, xs) => (Cons (x, (append xs l2))));

fun: Tree -> List:
toList t = (case t of 
                Leaf => (Nil) 
                | Node (x, l, r) => (append (toList l) (Cons (x, (toList r)))));


fun : List -> int -> int :
get l i = (case l of 
                Nil => ~1
                | Cons (x, xs) => if i == 0 
                                  then x 
                                  else (get xs (i - 1)));

fun : int -> Tree -> int:
kthSmallest k t = (get (toList t) k);

println (kthSmallest 3 t1);

println (kthSmallest 9 t2);