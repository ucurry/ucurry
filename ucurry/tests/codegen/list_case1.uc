int list ys = (case [1, 2, 3] 
               of [] => [int]
               | (x :: xs) => xs);


int single = (case [1, 2, 3] 
              of [] => 100
              | (x :: xs) => x);
println single;


int list empty = [int];
bool is_empty = (case empty 
                  of [] => true
                  | (x :: xs) => false);
println is_empty;

bool is_constant_empty = 
                (case [bool] 
                  of [] => true
                  | (x :: xs) => false);

println is_constant_empty;

check_type_error 
int undefined = (case [bool]
                 of [] => 0
                | (x :: xs) => 1 + x);

int zero = (case empty
                 of [] => 0
                | (x :: xs) => 1 + x);
println zero;
