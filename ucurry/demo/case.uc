-- pattern matching on primitive list, the patterns are special syntax just for primitive lists
fun : (int -> int) -> int list -> int list:
map f nums  = (case nums of
					          [] => [int]
                  | (h :: t) => (f h) :: (map f t));
int list powers = ((map (\(int->int) x -> x + 1)) [1, 2, 3]);