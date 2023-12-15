-- simple basic test
println if 1 < 1 then 1 else 2;
println if 1 == 1 then "equal" else "not equal";
println if true then 1 + 1 else 4 + 8;
println if false then false else true;

--  nested if
int y = if true then if true then 0 else 3 else  4;
println y;