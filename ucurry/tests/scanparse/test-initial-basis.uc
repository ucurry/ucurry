-- higher order functions
-- map function
datatype IntList = Nil | Cons of (int * IntList);

fun: (int -> int) -> IntList -> int list:
map f xs =  (case xs of 
                  Nil => []
                | Cons (x, y) => (f x) :: (map f y));

-- the following passes


-- but we want
-- fun: (int -> bool) -> IntList -> int list:
-- filter p ls = (case ls of 
--                 Nil => []
--               | Cons (y, ys) => if (p y) then y :: (filter p ys) else (filter p ys));
