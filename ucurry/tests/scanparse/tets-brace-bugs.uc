-- function type with braces ; 
(int -> int) -> int list -> int list x = map;

fun: int list -> int list:
map f xs =  (case xs of 
                  Nil => []
                | Cons a  => ((f a) cons (map f b)));

fun: int list -> int list:
map f xs =  (case xs of 
                  Nil => []
                | Cons y => (f y) :: (map f tl xs));