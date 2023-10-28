-- def -> expr -> let
let int x = 1 in x;
let int x = 1, bool v = 2 in if v then x else x;
let int x = 1 in x * x; 
(let int x = 1 in x);
(let int x = 1, bool v = 2 in if v then x else x);
(let int x = 1 in x * x);