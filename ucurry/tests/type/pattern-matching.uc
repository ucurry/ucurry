int x = (
    case (1, 2, 3) of   
     (a, b, c) => a
    | x => x.1
);

---- local name
--int y = (
    --case (1, 2, 3) of   
     --(a, b, c) => a
     --| x => x.1
--);

---- z unbound
--check_type_error int z = (
    --case (1, 2, 3) of   
     --(a, b, c) => a
     --| _ => z.1
--);

---- (a,b,c,d) and (1, 2, 3) will not match 
--check_type_error int f = (
    --case (1, 2, 3) of   
    --(a, b, c, d) => a
     --| x => x.1
--);

--int f = (
    --case (1, 2, 3) of   
    --(a, b, c) => a
--);

--datatype Color = Green | Red | Blue;

--Color c = (
    --case ((Green), 1) of 
       --(a, b) => a 
      --| x => x.0
--);


---- valid pattern matching with all pattern matched
--int c_int = (
    --case (Green) of 
       --Green => 1
     --| Red => 2
     --| Blue => 3
--);

---- valid pattern with catch all cases 
--int wildcard_int = (
    --case (Red) of 
       --Green => 1
     --| _ => 2
--);

---- invalid pattern without catch all cases 
--check_type_error int error_int = (
    --case (Green) of 
       --Green => 1
     --| Red => 2
--);

--datatype Number = ZERO 
                --| TIMESPLUS of (Number * int);

---- a tuple inside a value constructor must have a fall through case 
--check_type_error int n = (case (TIMESPLUS ((ZERO), 1)) of 
                            --ZERO => 0 
                            --| TIMESPLUS (ZERO , d) => d);

--int digit_a = (case (TIMESPLUS ((ZERO), 1)) of 
                          --ZERO => 0 
                        --| TIMESPLUS (ZERO , d) => d
                        --| TIMESPLUS a => a.1); 

--int digit_b = (case (TIMESPLUS ((ZERO), 1)) of 
                          --ZERO => 0 
                        --| TIMESPLUS (ZERO , d) => d
                        --| TIMESPLUS _ => 0); 

