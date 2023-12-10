fun : int list -> int : 
len l = (case l of [] => 0 
         | h :: t => 1 + (len t));
int empty_list_len = (len [int]);
int list_len = (len [1, 2, 3]);
println empty_list_len;
println list_len;
