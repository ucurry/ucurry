-- def -> datatype
datatype Color = Gray | White; 
datatype AlphaColor = Green of int 
                    | Red of int 
                    | Black of int; 
datatype Background = Pure of Color 
                    | Number of int;
check_type_error datatype Color = Purple; 
