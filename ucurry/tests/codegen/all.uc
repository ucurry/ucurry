--./binop.uc
println 1 + 1;
println 1 * 1; 
println 20 - 30;
println 10 / 2;

--./empty.uc


--./fun1.uc
fun: int -> int:
f x = x + 1;
println (f 2);


--./let.uc
let int x = 1 in println x;
let int y = 5, bool v = false in println y;
let int a = 3 in 
    let int b = 4 in 
        println a;


--./print-hello.uc
println "hello";
println "world";


--./vallet.uc
int x = 1;
let int x = 2 in println x;
println x;
let int y = 1 in println x;


--./valvar.uc
int a = 1;
bool b = true;
string str = "hello";
println str;
println b;
println a;

