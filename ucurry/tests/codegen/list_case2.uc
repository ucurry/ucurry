fun : int -> int :
pow n = n * n ;

fun : (int -> int) -> int list -> int list:
map f nums  = (case nums of [] => [int]
                 | h :: t => (f h) :: (map f t));
int list powers = ((map pow) [1, 2, 3]);

println hd powers;
println (hd (tl powers));
println (hd (tl (tl powers)));
println (null? (tl (tl (tl powers))));
