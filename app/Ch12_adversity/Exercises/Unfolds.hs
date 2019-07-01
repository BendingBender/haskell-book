module Ch12_adversity.Exercises.Unfolds where

import Lib
import Ch11_types.Exercises.BinaryTree (BinaryTree(Leaf, Node))

myIterate :: (a -> a) -> a -> [a]
myIterate makeNext curr = (curr:myIterate makeNext (makeNext curr))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr makeNext curr = case makeNext curr of
    Nothing -> []
    Just (a, b) -> (a:myUnfoldr makeNext b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold makeNext curr = case makeNext curr of
    Nothing -> Leaf
    Just (a, b, a') -> Node (unfold makeNext a) b (unfold makeNext a')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold makeNext 0
    where
        makeNext x
            | x < n = Just (x+1, x, x+1)
            | otherwise = Nothing

main :: IO ()
main = do
    test
        "take 10 $ myIterate (+1) 0"
        (take 10 $ myIterate (+1) (0::Int))
        [0,1,2,3,4,5,6,7,8,9]
    test
        "take 10 $ myUnfoldr (\\b -> Just (b, b+1)) 0"
        (take 10 $ myUnfoldr (\b -> Just (b, b+1)) (0::Int))
        [0,1,2,3,4,5,6,7,8,9]
    test
        "myUnfoldr (\\x -> if x < 2 then Just (x, x+1) else Nothing) 0"
        (myUnfoldr (\x -> if x < 2 then Just (x, x+1) else Nothing) (0::Int))
        [0,1]
    test
        "take 10 $ betterIterate (+1) 0"
        (take 10 $ betterIterate (+1) (0::Int))
        [0,1,2,3,4,5,6,7,8,9]
    test
        "treeBuild 0"
        (treeBuild 0)
        Leaf
    test
        "treeBuild 1"
        (treeBuild 1)
        (Node Leaf 0 Leaf)
    test
        "treeBuild 2"
        (treeBuild 2)
        (Node (Node Leaf 1 Leaf)
              0
              (Node Leaf 1 Leaf))
    test
        "treeBuild 3"
        (treeBuild 3)
        (Node (Node (Node Leaf 2 Leaf)
                    1
                    (Node Leaf 2 Leaf))
              0
              (Node (Node Leaf 2 Leaf)
                    1
                    (Node Leaf 2 Leaf)))
