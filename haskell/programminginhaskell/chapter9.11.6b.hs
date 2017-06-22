import Data.List (sortBy)
import Data.Function (on)


data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x >= y
valid Div x y = y /= 1 && x `mod` y == 0

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

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (l,r) <- split ns,
                     lx <- results l,
                     rx <- results r,
                     res <- combine lx rx]

exact_solutions :: [Int] -> Int -> [Expr]
exact_solutions ns target = [exp | ns' <- choices ns,
                             (exp,res) <- results ns',
                             res == target]

type Approx = (Expr, Int)

approx_solutions :: [Int] -> Int -> [Approx]
approx_solutions ns target = [(exp, abs (target - res)) | ns' <- choices ns,
                              (exp,res) <- results ns']

approx_solution :: [Int] -> Int -> Expr
approx_solution ns target = fst $ head (sortBy (compare `on` snd) (approx_solutions ns target))

solutions :: [Int] -> Int -> [Expr]
solutions ns target
    | length exact_sol > 0  = exact_sol
    | otherwise             = [approx_solution ns target]
    where exact_sol = exact_solutions ns target

-- Main

main :: IO()
main = print $ solutions [1,2,3,4] 29