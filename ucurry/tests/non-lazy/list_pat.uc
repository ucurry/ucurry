datatype Color = R | G | B ;

-- list pattern matching with datatype
println (case [(R), (G), (B)]
          of (R :: rest) => 2
          |  (G :: (B :: rest)) => 4
          |  (_ :: rest) => 3
          | [] => 5);

println (case [(B), (G), (B)]
          of (R :: rest) => 2
          |  (G :: (B :: rest)) => 4
          |  (_ :: rest) => 3
          | [] => 5);

println (case [(G), (B)]
          of (R :: rest) => 2
          |  (G :: (B :: [])) => 4
          |  (_ :: rest) => 3
          | [] => 5);

println (case [Color]
          of (R :: rest) => 2
          |  (G :: (B :: [])) => 4
          |  (_ :: rest) => 3
          | [] => 5);

-- list pattern matching on tuple 
(int * Color) list ys = [(107, (R)), (105, (B))];
println (case ys
          of ((x, B) :: rest) => x 
           | ((x, R) :: rest) => x
           | [] => 111 );

(int * Color) list xs = [(107, (R)), (105, (B))];
println (case xs
          of ((x, B) :: rest) => x 
           | ((x, R) :: rest) => (hd rest).0
           | [] => 111 );

println (case xs
          of ((x, B) :: rest) => x 
           | ((x, R) :: ((y, B) :: [])) => y
           | [] => 111 );