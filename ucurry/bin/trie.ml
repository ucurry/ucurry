type trie = Node of (string * trie) list | Leaf

let rec exists s trie =
  match (trie, s) with
  | _, [] -> true
  | Leaf, _ -> false
  | Node tries, c :: rest ->
      let search (c', tree) = c' == c && exists rest tree in
      List.exists (fun b -> b) @@ List.map search tries

let rec build_node s =
  match s with
  | [] -> Leaf
  | c :: rest ->
      let sub_node = build_node rest in
      Node [ (c, sub_node) ]

let rec add s trie =
  match (trie, s) with
  | _, [] -> trie
  | Leaf, xs -> build_node xs
  | Node tries, s ->
      let rec add_to_tries tries s =
        match (tries, s) with
        | _, [] -> tries
        | [], c :: cs -> [ (c, build_node cs) ]
        | (c, node) :: tries', c' :: cs' ->
            if c' == c then (c, add cs' node) :: tries'
            else (c, node) :: add_to_tries tries' s
      in
      Node (add_to_tries tries s)

let rec trie_string = function
  | Leaf -> ""
  | Node ps ->
      "["
      ^ String.concat ", \n"
          (List.map (fun (c, trie) -> c ^ "->" ^ trie_string trie) ps)
      ^ "]"
