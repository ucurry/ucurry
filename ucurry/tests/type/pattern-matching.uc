int x = (
    case (1, 2, 3) of   
     (a, b, c) => a
     | x => x.1
);

-- local name
int y = (
    case (1, 2, 3) of   
     (a, b, c) => a
     | x => x.1
);


int f = (
    case (1, 2, 3) of   
    (a, b, c) => a
);

datatype Color = Green | Red | Blue;

Color c = (
    case ((Green), 1) of 
       (a, b) => a 
      | x => x.0
);


-- valid pattern matching with all pattern matched
int c_int = (
    case (Green) of 
       Green => 1
     | Red => 2
     | Blue => 3
);

-- valid pattern with catch all cases 
int wildcard_int = (
    case (Red) of 
       Green => 1
     | _ => 2
);


datatype Number = ZERO 
                | TIMESPLUS of (Number * int);


int digit_a = (case (TIMESPLUS ((ZERO), 1)) of 
                          ZERO => 0 
                        | TIMESPLUS (ZERO , d) => d
                        | TIMESPLUS a => a.1); 

int digit_b = (case (TIMESPLUS ((ZERO), 1)) of 
                          ZERO => 0 
                        | TIMESPLUS (ZERO , d) => d
                        | TIMESPLUS (TIMESPLUS (ZERO, d), d2) => d2
                        | TIMESPLUS _ => 0); 


------- type-check for illegal pattern 
check_type_error (case 1 of (a, b) => a);
check_type_error (case 1 of ZERO => 1);
check_type_error (case (1, 2, 3, 4) of (a, b) => a);
check_type_error (case (Green) of ZERO => 1);