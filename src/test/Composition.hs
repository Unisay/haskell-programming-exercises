module Composition where

data A = A deriving (Eq, Show)
data B = B deriving (Eq, Show)
data C = C deriving (Eq, Show)
data D = D deriving (Eq, Show)
data E = E deriving (Eq, Show)


ab :: A -> B
ab _ = B

aBC :: A  -> (B, C)
aBC _ = (B, C)

aBCD :: A  -> (B, C, D)
aBCD _ = (B, C, D)

bc :: B -> C
bc _ = C

bcD :: B -> C -> D
bcD _ _ = D

bcdE :: B -> C -> D -> E
bcdE _ _ _ = E

ac :: A -> C
ac = bc . ab

cd :: C -> D
cd _ = D



(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

(|>>) :: (a -> (b, c)) -> (b -> c -> d) -> a -> d
(|>>) f g = uncurry g . f

(|>>>) :: (a -> (b, c, d)) -> (b -> c -> d -> e) -> a -> e
(|>>>) f g a = let (b, c, d) = f a in g b c d
