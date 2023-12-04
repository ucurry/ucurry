fun : int -> int -> int:
add2 x y = x + y;

-- semantic error: expect `res` to have `int -> int` type when only one argument is applied to `add2` 
int res = (add2 1); 