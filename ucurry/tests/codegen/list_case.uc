int list ys = (case [1, 2, 3] 
               of [] => [int]
               | x :: xs => xs);


int single = (case [1, 2, 3] 
              of [] => 100
              | x :: xs => x);
println single;


int list empty = [int];
bool is_empty = (case empty 
                  of [] => true
                  | x :: xs => false);
println is_empty;

bool is_constant_empty = 
                (case [bool] 
                  of [] => true
                  | x :: xs => false);

println is_constant_empty;

check_type_error 
int undefined = (case [int]
                 of [] => 0
                | x :: xs => 1 + x);

int zero = (case empty
                 of [] => 0
                | x :: xs => 1 + x);
println zero;

fun : int list -> int : 
len l = (case l of [] => 0 
         | h :: t => 1 + (len t));
int empty_list_len = (len [int]);
int list_len = (len [1, 2, 3]);
println empty_list_len;
println list_len;

fun : int -> int :
pow n = n * n ;

fun : (int -> int) -> int list -> int list:
map f nums  = (case nums of [] => [int]
                 | h :: t => (f h) :: (map f t));
int list powers = ((map pow) [1, 2, 3]);

println hd powers;
println (hd (tl powers));
println (hd (tl (tl powers)));
println (null? (tl (tl (tl powers))));

fun : (int -> bool) -> int list -> int list: 
filter f nums = (case nums of [] => [int]
                  | h :: t => if (f h) 
                              then (h :: (filter f t))
                              else (filter f t));
fun : int -> bool :
even num = (num % 2 == 0);
println (even 2);
println (even 1);

int list evens = (filter even [1, 2, 3, 4]);
println hd evens;
println (hd (tl evens));
println (null? (tl (tl evens)));