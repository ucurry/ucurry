fun : int -> int -> int :
addy y = \(int -> int) x -> x + y ;

fun : int -> int -> int -> int :
addz z = \(int -> int -> int) y -> (addy (y + z));

fun : int -> int -> int -> int -> int :
addx x = \(int -> int -> int -> int) z -> (addz (x + z));

int eight = (1 + (addy 3 4));
int six = (addz 1 2 3);
int ten = (addx (addy 2 3) 1 4 0);

println eight;
println six;
println ten;

--extra arguments 
fun : int -> int -> int -> int :
add_three a b c = a + b + c;
int -> int -> int apply_once = (add_three 1);
int -> int apply_twice = (apply_once 1);
int res = (apply_twice 1);
int add_three_ones = (((add_three 1) 1 ) 1);
println res;
println add_three_ones;

--mix
fun : int -> int -> int :
add_two x y = x + y;

fun : int -> int -> int :
curried_add x = \ (int -> int) y -> ((add_two x) y);
int eleven = ((curried_add  5) 6);
int nine = (curried_add 4 5);
println eleven;
println nine;
