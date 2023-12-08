datatype Color = Black of int | Red ; 
(int * int * Color ) triple = (1, 2, (Black 1));
int a = (case triple
          of  (a, b, Red) => a
          |   (a, b, Black s) => s);
println a;


--datatype Exp = Var of string 
             --| Set of (string * Exp)
             --| Function of (Exp * string  * Exp);

--Exp e = (Function ((Var "fun"), "x", (Set ("x", (Var "y")))));

--string fun_name = (case  e
                    --of Set (x, y) => x
                    --|  Function (Var x, _ , _) => x
                    --|  Var x => x);
--println fun_name;
(println(a));

int a = (let a = ((triple).0) 
            in (let b = ((triple).1) 
                    in (if ((((triple).2).T) == ("Red")) 
                        then (a) 
                        else (let a = (((triple).2).0) 
                                    in (let b = (((triple).2).1) 
                                        in (if (((((triple).2).2).T) == ("Black")) 
                                            then (let s = ((((triple).2).2)@Black) in (s)) 
                                            else (No match)))))));
(println(a));