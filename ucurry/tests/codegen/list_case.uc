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

