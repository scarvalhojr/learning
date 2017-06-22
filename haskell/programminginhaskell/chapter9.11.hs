
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where brak (Val n) = show n
                             brak e       = "(" ++ show e ++ ")"

e1 :: Expr
e1 = (App Add (Val 1) (App Mul (Val 2) (Val 3)))

e2 :: Expr
e2 = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o le re) = [apply o lv rv | lv <- eval le, rv <- eval re, valid o lv rv]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution exp ns target = elem (values exp) (choices ns) && eval exp == [target]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs,
             e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns target = [e | ns' <- choices ns, e <- exprs ns', eval e == [target]]

candidates :: [Int] -> [Expr]
candidates ns = [e | ns' <- choices ns, e <- exprs ns']

-- Section 9.8

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (l,r) <- split ns,
                     lx <- results l,
                     rx <- results r,
                     res <- combine' lx rx]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns target = [exp | ns' <- choices ns,
                        (exp,res) <- results ns',
                        res == target]

candidates' :: [Int] -> [Result]
candidates' ns = [r | ns' <- choices ns, r <- results ns']

-- Section 9.9

valid'' :: Op -> Int -> Int -> Bool
valid'' Add x y = x >= y
valid'' Sub x y = x > y
valid'' Mul x y = x /= 1 && y /= 1 && x >= y
valid'' Div x y = y /= 1 && x `mod` y == 0

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid'' o x y]

results'' :: [Int] -> [Result]
results'' []  = []
results'' [n] = [(Val n, n) | n > 0]
results'' ns  = [res | (l,r) <- split ns,
                     lx <- results'' l,
                     rx <- results'' r,
                     res <- combine'' lx rx]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns target = [exp | ns' <- choices ns,
                         (exp,res) <- results'' ns',
                         res == target]

candidates'' :: [Int] -> [Result]
candidates'' ns = [r | ns' <- choices ns, r <- results'' ns']

-- Main

main :: IO()
main = print $ last (solutions' [1,3,7,10,25,50] 765)

-- Exercise 1

choices' :: [a] -> [[a]]
choices' xs = [p | s <- subs xs, p <- perms s]

solutions''' :: [Int] -> Int -> [Expr]
solutions''' ns target = [exp | ns' <- choices' ns,
                         (exp,res) <- results'' ns',
                         res == target]

-- Exercise 2

rmfirst :: Eq a => a -> [a] -> [a]
rmfirst x []     = []
rmfirst x (y:ys)
    | x == y    = ys
    | otherwise = y : rmfirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice (x:xs) ys = elem x ys && isChoice xs (rmfirst x ys)

-- Exercise 3

split' :: [a] -> [([a],[a])]
split' []     = [([],[])]
split' (x:xs) = ([],x:xs) : [(x:ls,rs) | (ls,rs) <- split' xs]

exprs' :: [Int] -> [Expr]
exprs' []  = []
exprs' [n] = [Val n]
exprs' ns  = [e | (ls,rs) <- split' ns, l <- exprs' ls, r <- exprs' rs,
              e <- combine l r]

-- Exercise 4

all_exprs = candidates [1,3,7,10,25,50]
count_exprs = length all_exprs

valid_exprs = [e | e <- all_exprs, eval e /= []]
count_valid_exprs = length valid_exprs

-- Exercise 5

-- Exercise 6
