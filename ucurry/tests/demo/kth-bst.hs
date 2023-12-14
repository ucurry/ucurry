-- leetcode 230 https://leetcode.com/problems/kth-smallest-element-in-a-bst/

data Tree = Nil
          | Node Int Tree Tree
    deriving (Show, Read, Eq)

t1 = Node 3 (Node 1 Nil (Node 2 Nil Nil)) (Node 4 Nil Nil)

toList :: Tree -> [Int]
toList Nil = []
toList (Node x l r) = (toList l) ++ [x] ++ (toList r)

a = toList t1

kthSmallest :: Int -> Tree -> Int
kthSmallest k = flip (!!) (k - 1) . toList

