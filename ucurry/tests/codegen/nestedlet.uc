int b = 8;
let int x = 1 in let int x = 2 in print b;
let int x = 1 in let int x = 2 in print x;
let int x = 1 in (begin (let int x = 2 in print x), print x);
let int x = 1 in let int y = 2 in print(x + y);