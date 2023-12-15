datatype Num = One | Two | Three ;
datatype Color = G of Num | R of Num |  B of Num ;
(Num * Num * Num) scrutinee = ((One), (Two), (Three));
(Color * Color )  nested = ((G (One)), (B (Two)));

-- 
check_type_error (case nested of 
                      (G One, _) =>  1
                    | (G One, G Two) =>  2
                    | _ => 3);

 (case nested of 
                      (G One, G Two) =>  1
                    | (G One, _) =>  2
                    | _ => 3);
-- pattern unused
check_type_error (case scrutinee 
                    of (a, b, c) => 2
                    | _ => 1);

check_type_error (case scrutinee 
                    of (One, Two, _) => 1
                    | (One, Two, Three) => 2
                    | _ => 1);

check_type_error (case ((One), (Two), (Three)) 
            of (One, Three, Three) => 2
            | (One, Three, Two) => 3
            | (One, Two, Two) => 4
            | (One, Two, Three) => 5
            | (One, Two, Two) => 5
            | _ => 4);

check_type_error (case ((One), (Two), (Three)) 
            of (One, Three, Three) => 2
            | (One, Three, Two) => 3
            | (One, b, Two) => 4
            | (One, Two, Three) => 5
            | (One, Two, Two) => 5
            | _ => 4);

int b  = (case ((One), (Two), (Three)) 
            of (One, Three, Three) => 4
            |  (One, Three, Two) => 3
            |  (One, Two, One) => 4
            |  (One, Two, Three) => 2
            |  (One, Two, Two) => 6
            | _ => 4);
println b; 

 int c  = (case ((Three), (Three), (Three)) 
            of (a, b, Two) => 100
            | (One, Three, c) => 2
            | (One, Two, c) => 3
            | _ => 4);
println c; 


int d = (case ((G (One)), (R (Two)), (B (Three))) 
         of (G Two, R Two, _) => 1
          | (_, _, B Two) => 2
          | (G One, b, B Three) => 3
          | (G One, R Two, c ) => 4
          | _ => 4 );
println d;

datatype Number = ZERO 
                | TIMESPLUS of (Number * int);

int digit = (case (TIMESPLUS ((ZERO), 9)) of 
                TIMESPLUS (TIMESPLUS _, d) => d
              | TIMESPLUS (_, d) => d
              | ZERO => 0 );

println digit;