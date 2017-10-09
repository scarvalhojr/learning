
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x)     = Leaf (g x)
    fmap g (Node l r)   = Node (fmap g l) u(fmap g r)
