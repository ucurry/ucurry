datatype IntList = Nil | Cons of (int * IntList);

int hd_product = (case ((Cons (20, (Nil))), (Cons (10, (Nil))))
                    of (Nil, Cons (a, _)) => a
                    |  (Cons (a, _), Nil) => a
                    |  (Cons (a, _), Cons (b, _)) => a * b);

println hd_product;


datatype Number = One | Two | Three;
datatype Color =  Green of (Number * Number)| Blue of (Number * Number); 
Color color = (Blue ((Two), (Three)));
int c = (case color
          of Blue (Two, One) => 2
           | Blue (Two, Three) => 3
           | Blue b => 5
           | _ => 1);
println c;

(Number * Number) k = ((One), (Two));
int d = (case k of 
             (Two, One) => 2
           | (Two, Three) => 3
           | (One, Two) => 4
           | b => 5);
println d;