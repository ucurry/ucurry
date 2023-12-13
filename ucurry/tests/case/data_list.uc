datatype Graph =   Node of (int * int list) 
                 | StringNode  of (string * string list);
Graph g =  (Node (1, [10, 2, 3]));

int n = (case g of 
            Node (a, xs) => hd xs
          | Node (a, []) => 2
          | _ => 3);
println n;

bool is_empty = (case (Node (1, [int]))
                   of Node (_, [])  => true
                    | StringNode (_, []) => true
                    | _ => false);

println is_empty;
