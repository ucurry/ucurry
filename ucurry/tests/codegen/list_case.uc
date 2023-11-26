--int list ys = (case [1, 2, 3] 
               --of [] => []
               --| x :: xs => xs);


int single = (case [1, 2, 3] 
              of [] => 100
              | x :: xs => x);
println single;


int list empty = [];
bool is_empty = (case empty 
                  of [] => true
                  | x :: xs => false);
println is_empty;

bool is_constant_empty = 
                (case [] 
                  of [] => true
                  | x :: xs => false);
println is_constant_empty;

check_type_error 
int undefined = (case []
                 of [] => 0
                | x :: xs => 1 + x);

int zero = (case empty
                 of [] => 0
                | x :: xs => 1 + x);
println zero;

fun : int list -> int : 
len l = (case l of [] => 0 
         | h :: t => 1 + (len t));
int empty_list_len = (len []);
int list_len = (len [1, 2, 3]);
println empty_list_len;
println list_len;

fun : int -> int :
pow n = n * n ;

fun : (int -> int) -> int list -> int list:
pows f nums  = (case nums of [] => []
                 | h :: t => (f h) :: (pows f t) );
int list powers = ((pows pow) [1, 2, 3]);
println hd powers;
println (hd (tl powers));
println (hd (tl (tl powers)));
println (null? (tl (tl (tl powers))));
