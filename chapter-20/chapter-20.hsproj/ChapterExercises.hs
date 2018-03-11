{-# OPTIONS_GHC -Wall #-}

module ChapterExercises where
  
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b
  
-- (1)
data Constant a b = Constant b
instance Foldable (Constant a) where
  foldMap f (Constant a) = f a
  foldr f b (Constant a) = f a b

-- (2)
data Two a b = Two a b
instance Foldable (Two a) where
  foldMap f (Two _ a) = f a
  foldr f b (Two _ a) = f a b

-- (3)
data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldMap f (Three _ _ a) = f a
  foldr f b (Three _ _ a) = f a b

-- (4)
data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' _ a b) = mappend (f a) (f b)
  foldr f b (Three' _ a1 a2) = f a2 $ f a1 b

-- (5)
data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldMap f (Four' _ a b c) = mappend (mappend (f a) (f b)) (f c)
  foldr f base (Four' _ a b c) = f c $ f b $ f a base


-- Filter (tricky)
filterF :: (Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF condition xs = foldr next mempty xs where
  next x = if (condition x) then mappend (pure x) else id