datatype Color = Black of int | Red ; 

Color b = (Black 1);
println (case b of Black a => a
            | _ => 3);

(int * Color) p = (2, (Black 3));
println (case p of (a, Black b) => b
                | _ => 3);  

(int * int * Color) t = (1, 2, (Black 3));
println (case t of (a, b, Black s) => s
            | _ => 2);

int a = (case (1, 2, (Black 1))
          of  (a, b, Red) => a
          |   (a, b, Black s) => s
          |    _ => 3);
println a;


datatype Exp = Var of string 
             | Set of (string * Exp)
             | Function of (Exp * string  * Exp);

Exp e = (Function ((Var "fun"), "x", (Set ("x", (Var "y")))));

string fun_name = (case  e
                    of Set (x, y) => x
                    |  Function (Var x, _ , _) => x
                    |  Var x => x
                    |  _ => "hello");
println fun_name;