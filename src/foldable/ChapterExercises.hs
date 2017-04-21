module ChapterExercises where 
  
     
      
      
data Constant a b = Constant a   
  deriving (Eq, Show)  

instance Foldable (Constant a) where
  foldMap _ _ = mempty 
  foldr _ z _ = z


data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two b) where
  foldMap f (Two _ y) = f y
  foldr f z (Two _ y) = f y z


data Three a b c = Three a b c
  deriving (Eq, Show)
  
instance Foldable (Three a b) where
  foldMap f (Three _ _ y) = f y
  foldr f z (Three _ _ y) = f y z


data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ y y') = f y `mappend` f y'
  foldr f z (Three' _ y y') = f y . f y' $ z


data Four' a b = Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ y1 y2 y3) = f y1 `mappend` f y2 `mappend` f y3
  foldr f z (Four' _ y1 y2 y3) = f y1 . f y2 . f y3 $ z


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)