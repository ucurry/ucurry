--the following code should fail the semantic check in `clconver.ml`
int x = 1;
fun: int -> int:
f a = ((\ (int -> int) x -> x = a) 1);

-- why does this test does not fail in clconvert?
println (f 4);
println x;

int y = (let x = 1 in x = 2);
println y;
