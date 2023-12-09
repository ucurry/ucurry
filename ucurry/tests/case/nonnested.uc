datatype Num = One | Two | Three ;
datatype ColorN = G of Num | R of Num |  B of Num ;

datatype Color = Green of int | Red of int | Yellow of int ;
--int a = (case (Yellow 10)
 --of Green a => 1
 --|  Yellow b => b
 --|  Red c => 2
 --);
--(println a);

int b  = (case (R (Two)) 
            of G One => 100
            | G _ => 2
            | B _ => 3
            | _ => 4);
(println b);




-- is_exhaustive 
-- exhaustive case without wildcard -> what should be the tree looks like 
-- no tuple 