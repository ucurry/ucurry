datatype List = Nil | Cons of int * List;

List empty = (Nil);
List singleton = (Cons (1, (Nil)));

List another = (Cons (1, empty));