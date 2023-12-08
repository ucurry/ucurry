datatype Num = Zero | Addone of Num;

(case (Addone (Addone (Zero))) of 
    Zero => 2
  | Addone (Addone (Zero)) => (println 3));