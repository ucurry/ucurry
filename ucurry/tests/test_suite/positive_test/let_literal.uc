int list test_list = [1, 2, 3];
int res = 
if (null? test_list) 
then 1
else let h = hd test_list, 
         t = tl test_list 
     in hd t; 
println res;

int list empty = [int];
int res_prime = 
if (null? empty) 
then ~100
else let h = hd test_list, 
         t = tl test_list 
     in hd t; 
println res_prime;
