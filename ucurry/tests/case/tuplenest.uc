--datatype Color = Black of int | Red ; 

--int a = (case (1, 2, (Black 1))
          --of  (a, b, Red) => a
          --|   (a, b, Black s) => s);
--println a;


datatype Exp = Var of string 
             | Set of (string * Exp)
             | Function of (Exp * string  * Exp);

Exp e = (Function ((Var "fun"), "x", (Set ("x", (Var "y")))));

string fun_name = (case  e
                    of Set (x, y) => x
                    |  Function (Var x, _ , _) => x
                    |  Var x => x);
println fun_name;