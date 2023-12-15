
datatype Num = One | Two | Three ;
datatype Color = G of Num | R of Num |  B of Num ;
(Num * Num * Num) scrutinee = ((One), (Two), (Three));
(Color * Color )  nested = ((G (One)), (B (Two)));
(Num * (Num * Num)) num_nested = ((One), ((One), (One)));

check_type_error (case nested of (G One, G Two) =>  1
               | (G One, _) =>  2
               | (G One, G One) =>  2
               | _ => 3);

check_type_error (case nested of (G One, G Two) =>  1
               | (_, G One) =>  2
               | (G One, G One) =>  2
               | _ => 3);

check_type_error (case nested of 
                 (G One, G Two) =>  1
               | (_, G One) =>  2
               | (G Two, G One) =>  2
               | _ => 3);

check_type_error (case scrutinee of 
                  (_, Two, _) => 1
                | (One, Two, Three) => 3
                | (One, Three, _) => 2
                | _ => 3);

check_type_error (case num_nested of 
                      (One, (One, One)) => 1
                    | (One, (One, One)) => 2
                    | _ => 3);


println (case num_nested of 
                      (One, (One, One)) => 1
                    | (_, (One, One)) => 2
                    | _ => 3);


println (case num_nested of 
          (One, (_, One)) => 1
        | (_, (One, One)) => 2
        | _ => 3);

println (case nested of 
          (G One, G Two) =>  1
        | (G One, _) =>  2
        | _ => 3);

