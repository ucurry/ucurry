int x = 1;
fun : int -> int :
addx y = y + x;


fun : (int -> int) -> int list -> int list:
map f nums  = (case nums of [] => [int]
                 | (h :: t) => (f h) :: (map f t));

int list -> int list  map_add_one = (map addx);
int list orig = [1, 3];
int list mapped = (map_add_one orig);

println hd mapped;
println (hd (tl mapped));
println (null? (tl (tl mapped)));

