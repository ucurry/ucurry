datatype Num = Zero | Addone of Num;

(case (Zero) of 
    Addone (Addone (Zero)) => 0
  | _ => 1
  | Zero => 2);
