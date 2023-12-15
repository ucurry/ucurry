datatype Num = Zero | Addone of Num;

--Num zero = (Zero);
--Num one = (Addone zero);

int a = (if (Zero).T == "Addone"
then let a = (Zero)@Zero in 0
else 1);

println a;
println (let a = (Zero)@Addone in 0);
println (let a = (Addone (Zero))@Zero in 0);