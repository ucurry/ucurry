
datatype Color = Green | Red | Blue;


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