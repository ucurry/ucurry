fun: string -> int -> int:
f name age = age + 10;

int aAge = 10;
int bAge = 27;
string alpha = "alpha";
string beta = "beta";

fun: (string -> int -> int) -> (int -> int):
setAlpha f = (f alpha);

fun: (string -> int -> int) -> (int -> int):
setBeta f = (f beta);

fun: (int -> int) -> int:
alphaAge f = (f aAge);

fun: (int -> int) -> int:
betaAge f = (f bAge);

println (alphaAge (setAlpha f));
println (betaAge (setBeta f));



