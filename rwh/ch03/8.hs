data Tree a = Node a (Tree a) (Tree a)
            | Empty
                          deriving (Show)

--treeHeight :: (Num n, Ord n) => Tree t -> n
treeHeight Empty = 0
treeHeight (Node x a b) = 1 + max (treeHeight a) (treeHeight b)
