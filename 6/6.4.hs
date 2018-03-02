data BinaryTree = Root
                | Leaf { parent :: BinaryTree}
                | Node { value :: Integer,
                         left :: BinaryTree,
                         right :: BinaryTree,
                         parent :: BinaryTree
                       }


insert :: BinaryTree -> Integer -> BinaryTree
insert Root x = let n = Node x (Leaf n) (Leaf n) Root in n 
insert (Leaf p) x = let n = Node x (Leaf n) (Leaf n) p in n
insert node@(Node v l r p) x | x < v = let n = Node v (insert' l x n) r p in n
                             | otherwise = let n = Node v l (insert' r x n) p in n
insert' (Leaf p) x p0 = let n = Node x (Leaf n) (Leaf n) p0 in n
insert' node@(Node v l r p) x p0 | x < v = let n = Node v (insert' l x n) r p0 in n
                                 | otherwise = let n = Node v l (insert' r x n) p0 in n


remove :: BinaryTree -> Integer -> BinaryTree
remove Root _ = Root
remove l@(Leaf _) _ = l
remove node@(Node v l r p) x | x < v = let n = Node v (remove' l x n) r p in n
                             | x > v = let n = Node v l (remove' r x n) p in n
                             | otherwise = concat' p l r

remove' l@(Leaf _) x p0 = Leaf p0
remove' node@(Node v l r p) x p0 | x < v = let n = Node v (remove' l x n) r p0 in n
                                 | x > v = let n = Node v l (remove' r x n) p0 in n
                                 | otherwise = concat' p0 l r

concat' Root (Leaf _) (Leaf _) = Root
concat' p (Leaf _) (Leaf _) = Leaf p
concat' p (Leaf _) (Node v l r p0) = Node v l r p
concat' p (Node v l r p0) t = let n = Node v l (concat' n r t) p in n


containsElement :: BinaryTree -> Integer -> Bool
containsElement (Node v l r _) x | x < v = containsElement l x
                                 | x > v = containsElement r x
                                 | otherwise = True
containsElement _ _ = False