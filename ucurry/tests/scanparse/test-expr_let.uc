-- def -> expr -> let
let x = 1 in x;
let x = 1, v = false in if v then x else x;
let x = 1 in x * x; 
(let x = 1 in x);
(let x = 1 in x * x);
