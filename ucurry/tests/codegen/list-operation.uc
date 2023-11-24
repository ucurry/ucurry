---- hd and tl
int x = hd [1, 2];
println x;
int y = hd [4, 2, 3, 4];
println y;
println hd [5, 6, 7];
int list hd_list = hd [[1, 2], [3, 4]];
int hd_elem = hd hd_list;
println hd_elem;
int list t = tl [2, 3, 4, 5];
int h = hd t; 
println h;
string list ss = ["hello", "world"];
string s = hd ss;
println s;

---- cons 
int list tl_list = [1, 2, 3];
int front = 1;
int list cons_list = front :: tl_list;
println (hd cons_list);

---- null
println (null? []);
println (null? [1, 2]);
println (null? (tl [1]));
println (null? (tl [1, 2, 3]));
