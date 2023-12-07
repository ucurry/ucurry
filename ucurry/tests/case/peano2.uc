datatype Num = Zero | Addone of Num;

--Num zero = (Zero);
--Num one = (Addone zero);

--if (Zero).T == "Addone" 
--then let a = (Zero)@2 in 0
--else 1;

let a = (Zero)@Addone in 0;