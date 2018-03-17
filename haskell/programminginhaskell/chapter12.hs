
-- 1.

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf          = Leaf
  fmap g (Node l x r)  = Node (fmap g l) (g x) (fmap g r)

-- 2.

-- instance Functor ((-->) a) where
--   -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- 3.

-- instance Applicative ((->) a) where
--   -- pure :: b -> (a -> b)
--   pure = const
--
--   -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
--   f (<*>) g = \x -> f x (g x)

-- 4.

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (Z xs) = Z (fmap f xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]

-- > [(+1),(+2)] <*> [4,5,6]
-- [5,6,7,6,7,8]
-- > Z [(+1),(+2)] <*> Z [4,5,6]
-- Z [5,7]

-- 5.

x :: Applicative f => f (a -> b)
x = undefined

y :: Applicative f => f (c -> a)
y = undefined

z :: Applicative f => f c
z = undefined

-- x <*> (y <*> z)       ==  (pure (.) <*> x <*> y) <*> z
-- Applicative f => f b  ==  Applicative f => f b

-- 6.

-- instance Monad ((->) a) where
--   f >>= k = \ r -> k (f r) r

-- 7.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Var x) = Var (f x)
  fmap _ (Val x) = Val x
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
  pure x = Var x

  -- ???
  Var f <*> Var x    = Var (f x)
  f <*> (Add e1 e2)  = Add (f <*> e1) (f <*> e2)
  _ <*> Val x        = Val x

instance Monad Expr where
  (>>=) = undefined  -- ???

-- 8.

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- ???
  fmap g st = undefined -- do...

instance Applicative ST where
  pure x = S (\s -> (x, s))

  -- ???
  stf <*> stx = undefined -- do...

instance Monad ST where
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')
