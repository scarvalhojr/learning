type Birds = Int  
type Pole = (Birds,Birds)

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

--

foo :: Maybe String
foo = do
--    x <- Nothing :: Maybe String
    x <- Just 3
    (y:y':ys) <- Just "!"
    Just (show x ++ ys)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start >>= landRight 1
    second <- landRight 2 first
    landLeft 1 second
