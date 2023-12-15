string s = "hello world!";  -- assign to global variable
println s;                  -- unary operator

let a = "hello ",
    b = "world"
in (begin print a, println b);         -- let expressions and sequencing

if true then "hello" else "world";     -- if expressions