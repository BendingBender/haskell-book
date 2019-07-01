module Ch11_types.Exercises.BinaryTree where

import Lib

data BinaryTree a =
    Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a
    => a
    -> BinaryTree a
    -> BinaryTree a
insert' a Leaf = Node Leaf a Leaf
insert' a (Node left a' right)
    | a < a' = Node (insert' a left) a' right
    | a > a' = Node left a' (insert' a right)
    | otherwise = Node left a' right

mapTree :: (a -> b)
    -> BinaryTree a
    -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder tree = go [] tree
    where
        go list Leaf = list
        go list (Node left a right) = (a:(go (go list right) left))

inorder :: BinaryTree a -> [a]
inorder tree = go [] tree
    where
        go list Leaf = list
        go list (Node left a right) = go (a:(go list right)) left

postorder :: BinaryTree a -> [a]
postorder tree = go [] tree
    where
        go list Leaf = list
        go list (Node left a right) = go (go (a:list) right) left

foldTree :: (a -> b -> b)
    -> b
    -> BinaryTree a
    -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = foldTree f (f a (foldTree f z right)) left
-- foldTree f z tree = foldr f z (inorder tree)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
    Node (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

testTree :: BinaryTree Integer
testTree =
    Node
        (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

main :: IO ()
main = do
    test
        "insert' 5 (insert' 3 (insert' 0 Leaf))"
        (insert' 5 (insert' 3 (insert' (0::Int) Leaf)))
        (Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf)))
    test "mapTree (+1) testTree'" (mapTree (+1) testTree') mapExpected
    test "preorder testTree" (preorder testTree) [2, 1, 3]
    test "inorder testTree" (inorder testTree) [1, 2, 3]
    test "postorder testTree" (postorder testTree) [1, 3, 2]
    test "foldTree (+) 0 testTree" (foldTree (+) 0 testTree) 6
