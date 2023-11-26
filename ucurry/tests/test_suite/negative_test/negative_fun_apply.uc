-- semantic error: apply three arguments to a function waiting for two arguments
fun : int -> int -> int:
add2 x y = x + y;
(add2 1 2 3);

-- semantic error: expect `res` to have `int -> int` type when only one argument is applied to `add2` 
int res = (add2 1); 