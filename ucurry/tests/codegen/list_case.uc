int list ys = (case [1, 2, 3] 
               of [] => []
               | x :: xs => xs);

int single = (case [1, 2, 3] 
              of [] => 100
               | x :: xs => x);
println single;

