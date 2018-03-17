
import Data.Foldable

-- 1.

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   mempty = (mempty, mempty)
--   (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- 2.

-- instance (Monoid b) => Monoid (a -> b) where
--   mempty = \_ -> mempty
--   f `mappend` g = \x -> f x `mappend` g x

-- 3.

data Perhaps a = NoWay | Okay a
  deriving Show

instance Functor Perhaps where
  fmap _ NoWay    = NoWay
  fmap f (Okay x) = Okay (f x)

instance Applicative Perhaps where
  pure x  = Okay x

  Okay f <*> Okay x = Okay (f x)
  _      <*> _      = NoWay

instance Foldable Perhaps where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap _ NoWay    = mempty
  foldMap f (Okay x) = f x

  -- fold :: (Monoid m, Foldable t) => t m -> m
  fold NoWay    = mempty
  fold (Okay x) = x

  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr _ z NoWay    = z
  foldr f z (Okay x) = f x z

  -- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
  foldl _ z NoWay    = z
  foldl f z (Okay x) = f z x

instance Traversable Perhaps where
  -- traverse :: Applicative f => (a -> f b) -> Perhaps a -> f (Perhaps b)
  traverse f NoWay    = pure NoWay
  traverse f (Okay x) = pure Okay <*> f x

  -- sequenceA :: Applicative f => Perhaps (f a) -> f (Perhaps a)
  sequenceA NoWay    = pure NoWay
  sequenceA (Okay x) = pure Okay <*> x

-- 4.

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
  pure x = Node Leaf x Leaf

  Node _ f _ <*> Node l x r = Node (pure f <*> l) (f x) (pure f <*> r)
  _          <*> _          = Leaf

instance Foldable Tree where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> Tree a -> m
  foldMap _ Leaf         = mempty
  foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

instance Traversable Tree where
  -- -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  -- traverse f (Node l x r) = -- ???

  -- sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA Leaf         = pure Leaf
  -- sequenceA (Node l x r) = -- ??

-- 5.

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])
