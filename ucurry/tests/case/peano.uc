datatype Num = Zero | Addone of Num;

Num zero = (Zero);
Num one = (Addone zero);

(case (Zero) of 
    Addone (a) => (println 0)
  | _ => (println 1));

(println
  (case (Addone (Zero)) of 
      Addone (Zero) => 2
    | _ => 10
    | Zero => 20));

(println
  (case (Addone (Addone (Zero))) of 
      Zero => 2
    | Addone (Addone (Zero)) => 3));

(println
  (case (Addone (Addone (Zero))) of 
      _ => 4
    | Zero => 2
    | Addone (Addone (Zero)) => 3));

