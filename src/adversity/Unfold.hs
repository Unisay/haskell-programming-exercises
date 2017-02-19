module Unfold where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Just (a, b') -> a : myUnfoldr f b'
  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Just (l, b, r) -> Node (unfold f l) b (unfold f r)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild i = unfold f 0 where
  f a
    | n <= i = Just (n, a, n)
    | otherwise = Nothing
    where n = a + 1
