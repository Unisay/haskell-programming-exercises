module LibEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr f [] where
  f (Left a) l = a : l
  f (Right b) l = l

rights' :: [Either a b] -> [b]
rights' = foldr f [] where
  f (Left a) l = l
  f (Right b) l = b : l

-- $ partitionEithers' [Left 1, Right 2, Right 3, Left 4]
-- ([1,4],[2,3])
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a) = fl a
either' _ fr (Right b) = fr b

-- Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fbc = either' (const Nothing) (Just . fbc)
