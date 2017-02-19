module LibMaybe where

-- Simple boolean checks for Maybe values.
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing = b

-- In case you just want to provide a fallback value.
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe d Nothing = d

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d = mayybee d id

-- Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x: _) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (: [])

--For when we just want to drop the Nothing values from our list.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms
  | all isJust ms = Just (catMaybes ms)
  | otherwise = Nothing
