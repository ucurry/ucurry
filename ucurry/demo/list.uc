-- to create an empty list, the type of the list needs to go into the empty list brackt 
int list empty = [int]; 

-- a non empty integer list 
int list xs = [1, 2]; 
int list ys = 1 :: [2]; 

-- a nested list 
int list list zs = [[1], [1,2,3], [1]]; 

-- `hd` operator can be used to access the first element of the list 
println hd xs; 

-- `tl` operator can be used to access the rest element of the list 
println hd (tl xs); 

-- `null?' operator can be used test if the list is empty 
println null? xs; 
println null? empty;
