datatype Color = Red | Black; 
datatype RBTree = E | T of (Color * RBTree * int * RBTree);

(Color * RBTree * int * RBTree ) tree = ((Black), (E), 2, (E));
(Color * RBTree * int * RBTree ) tree1 = ((Black), (T ((Red), (E), 1, (T ((Red), (E), 1, (E))))), 2, (E));
(Color * RBTree * int * RBTree ) tree2 = ((Black), (T ((Black), (E), 1, (E))), 2, (T ((Red), (E), 1, (T ((Red), (E), 1, (E))))));
(Color * RBTree * int * RBTree ) tree3 = ((Black), (T ((Red), (T ((Red), (E), 1, (E))), 1, (E))), 2, (E));

int ten = ( case tree of 
             (Black, T _, a, E) =>  1
           | (Red, _, a, b) => 2 
           | (Black, E, _, T _) => 8
           | (_, E, _, E) => 10
           | (Black, T _, a, T _) => 9
           | _ => 12
);

int six = ( case tree of 
             (Black, T _, a, E) =>  1
           | (Red, _, a, b) => 2 
           | (Black, E, _, T _) => 8
           | (_, E, _, T _ ) => 10
           | (Black, E, a, E) => 6
           | _ => 12
);

int zero = ( case tree3 of 
          (Black, T (Red, T (Red, a, x, b), y, c), z, d) => 0
        | (Black, T (Red, a, x, T (Red, b, y, c)), z, d) => 1
        | (Black, a, x, T (Red, T (Red, b, y, c), z, d)) => 2
        | (Black, a, x, T (Red, b, y, T (Red, c, z, d))) => 3
        | _ => 4
);

int one = ( case tree1 of 
          (Black, T (Red, T (Red, a, x, b), y, c), z, d) => 0
        | (Black, T (Red, a, x, T (Red, b, y, c)), z, d) => 1
        | (Black, a, x, T (Red, T (Red, b, y, c), z, d)) => 2
        | (Black, a, x, T (Red, b, y, T (Red, c, z, d))) => 3
        | _ => 4
);

int three = ( case tree2 of 
          (Black, T (Red, T (Red, a, x, b), y, c), z, d) => 0
        | (Black, T (Red, a, x, T (Red, b, y, c)), z, d) => 1
        | (Black, a, x, T (Red, T (Red, b, y, c), z, d)) => 2
        | (Black, a, x, T (Red, b, y, T (Red, c, z, d))) => 3
        | _ => 4
);


int four = ( case tree of 
          (Black, T (Red, T (Red, a, x, b), y, c), z, d) => 0
        | (Black, T (Red, a, x, T (Red, b, y, c)), z, d) => 1
        | (Black, a, x, T (Red, T (Red, b, y, c), z, d)) => 2
        | (Black, a, x, T (Red, b, y, T (Red, c, z, d))) => 3
        | _ => 4
);

println zero;
println one;
println three;
println four;
println six;
println ten;