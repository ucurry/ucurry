datatype Num = Zero | Addone of Num;

Num zero = (Zero);
Num one = (Addone zero);

(case (Zero) of 
    Addone a => (println 0)
  | _ => (println 1));

(println
  (case (Addone (Zero)) of 
      Addone Zero => 2
    | Zero => 20
    | _ => 10));


(println
  (case (Addone (Addone (Zero))) of 
      Zero => 2
    | Addone Addone Zero => 3
    | _ => 0));
