datatype Num = Zero | Addone of Num;

(println
  (case (Addone (Addone (Zero))) of 
      Zero => 2
    | Addone Addone Zero => 3));
