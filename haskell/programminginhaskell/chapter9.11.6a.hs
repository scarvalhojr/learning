
data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Integer -> Integer -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x >= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Integer | App Op Expr Expr

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
ops = [Add, Sub, Mul, Div, Exp]

type Result = (Expr, Integer)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Integer] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (l,r) <- split ns,
                     lx <- results l,
                     rx <- results r,
                     res <- combine lx rx]

solutions :: [Integer] -> Integer -> [Expr]
solutions ns target = [exp | ns' <- choices ns,
                       (exp,res) <- results ns',
                       res == target]

candidates :: [Integer] -> [Result]
candidates ns = [r | ns' <- choices ns, r <- results ns']

-- Main

main :: IO()
main = print $ solutions [3,1,4,2] 2417851639229258349412353
