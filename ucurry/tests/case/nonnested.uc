datatype Num = One | 
datatype Color = Green of int | Red of int | Yellow of int ;
int a = (case (Green 1)
 of Green 1 => 1
 |  _ => 100
 |  Green _ => 2
 |  Red 1 => 10
 );

(println a);

-- is_exhaustive 
-- exhaustive case without wildcard -> what should be the tree looks like 
-- no tuple 