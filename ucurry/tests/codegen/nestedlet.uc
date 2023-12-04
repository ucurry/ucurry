int b = 8;
let x = 1 in let x = 2 in print b;
let x = 1 in let x = 2 in print x;
let x = 1 in (begin (let x = 2 in print x), print x);
let x = 1 in let y = 2 in print(x + y);