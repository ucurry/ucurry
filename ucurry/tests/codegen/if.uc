-- simple basic test
println if 1 < 1 then 1 else 2;
println if 1 == 1 then "equal" else "not equal";
println if true then 1 + 1 else 4 + 8;
println if false then false else true;

-- mutation and nested if
int x = 1;
if true then x = 2 else x = 3;
println x;

int y = 5;
if false then y = 2 else y = 3;
println y;

if true then if true then y = 0 else y = 3 else y = 4;
println y;