
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
  deriving Show

exec :: Code -> Stack -> Stack
exec []          s       = s
exec (PUSH n:cs) s       = exec cs (n:s)
exec (ADD:cs)    (n:m:s) = exec cs (m + n:s)
exec (ADD:_)     _       = error "ADD operation failed: empty stack"

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

e1 = Add (Add (Val 2) (Val 3)) (Val 4)

comp' :: Expr -> Code
comp' = comp'' []
  where comp'' acc (Val n)   = PUSH n : acc
        comp'' acc (Add x y) = comp'' (comp'' (ADD : acc) y) x
