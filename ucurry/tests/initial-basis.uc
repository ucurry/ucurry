-- higher order functions
-- map function
-- cannot parse (int -> int) -> int list -> int list
-- fun: int list -> int list:
-- map f xs =  (case xs of 
--                   Nil => []
--                 | Cons y ys => (f y) :: a);

-- the following passes
fun: int -> bool -> int list -> int list:
filter p ls = (case ls of 
                  Nil => []
                | Cons ys => if (p y) then y :: (filter p ys) else (filter p ys));

-- but we want
-- fun: (int -> bool) -> int list -> int list:
-- filter p ls = (case ls of 
--                   Nil => []
--                 | Cons y ys => if (p y) then y :: (filter p ys) else (filter p ys));
