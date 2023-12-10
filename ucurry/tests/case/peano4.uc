datatype Num = Zero | Addone of Num;


(case (Addone (Addone (Zero))) of 
    Zero => (Zero)
    | Addone Addone Zero => (Zero));
