datatype Num = Zero | Addone of Num;

Num zero = (Zero);
Num one = (Addone zero);

(case (Zero) of 
    Addone (Addone (Zero)) => 0
  | _ => 1
  | Zero => 2);
