datatype Trie = Node of (string * Trie) list | Leaf ;

fun : ((string * Trie) -> int)  -> (string * Trie) list  -> int list :
map_int f children = (case children 
                            of  [] => [int]
                            | c :: cs => (f c) :: (map_int f cs));

fun : ((string * Trie) -> bool)  -> (string * Trie) list  -> bool list :
map_bool p children = (case children 
                            of  [] => [bool]
                            | c :: cs => (p c) :: (map_bool p cs));

fun : ((string * Trie) -> bool) -> (string * Trie) list -> bool :
exists p children = (case children 
                     of [] => false
                     | c :: cs => (p c) or (exists p cs));

fun : string list -> Trie :
build_node s = (case s 
                of [] => (Leaf)
                | c :: cs => let sub_node = (build_node cs) 
                             in (Node [(c, sub_node)]));

fun : Trie -> int list:
print_trie s = (case s 
                of Leaf => [(println "")]
                | Node ps => 
                    let print_node = \((string * Trie) -> int) node -> 
                        (begin print "<", print node.0, print "-", 
                        (print_trie node.1), print "")
                    in 
                        (map_int print_node ps));
                        
fun : bool list -> bool: 
any_true booleans = (case booleans 
                    of [] => false
                    | b :: bs => b or (any_true bs));

fun : string list -> Trie -> bool :
string_exists s trie = 
    (case (trie, s) 
     of (_, []) => true            
      | (Leaf, _) => false 
      | (Node tries, c :: rest) => 
        let search = \((string * Trie) -> bool) 
            node -> (node.0 == c and (string_exists rest node.1))
        in (any_true (map_bool search tries)));

-- this does not work since no let rec is supported
fun : string list -> Trie -> Trie :
add s trie =
    (case (trie, s) 
      of (_, []) => trie
      | (Leaf, xs) => (build_node s)
      | (Node tries, s) => 
        let add_to_tries = \((string * Trie) list -> string list -> (string * Trie) list)
            t s -> (case (t, s)  
                    of (t, []) => t 
                    |  ([], c :: cs) => [(c, (build_node cs))]
                    |  (node :: rest, c2 :: cs) => 
                        if node.0 == c2 
                        then (node.0, (add cs node.1)) :: rest
                        else node :: (add_to_tries rest s))
        in (Node (add_to_tries tries s))
    );

string list hello =["h", "e", "l", "l", "o"];
Trie trie = (build_node hello);
Trie trie2 = (add hello (Leaf));
(print_trie trie);

println (string_exists [string] trie);
println (string_exists [string] (Leaf));
println (string_exists ["h"] (Leaf));
println (string_exists ["h"] trie);
println (string_exists ["h", "e"] trie);
println (string_exists ["h", "i"] trie);
println (string_exists ["h", "e", "l", "l", "o"] trie);
println (string_exists ["h", "e", "l", "l", "o"] trie2);
(print_trie (add ["h", "i"] (Leaf)));
--(print_trie (add ["h", "i"] trie));

