import AST
import Test.QuickCheck
import System.Random

rBinOp :: Integer -> Gen BinaryOperation
rBinOp seed = variant seed (elements [Add, Sub, Mul, Div, Min, Max, Copysign])

rUnOp :: Integer -> Gen UnaryOperation
rUnOp seed = variant seed (elements [Abs, Neg, Sqrt, Ceil, Floor, Trunc, Nearest])

rRelOp :: Integer -> Gen RelationalOperation
rRelOp seed = variant seed (elements [Eq, Ne, Lt, Gt, Le, Ge])

rConst :: Int -> (Expression, StdGen)
rConst seed = (Const d, g)
  where
    (d, g) = random (mkStdGen seed)

rParam :: Int -> Int -> (Expression, StdGen)
rParam m seed = (Param i, g)
  where
    (i, g) = randomR (0, m-1) (mkStdGen seed)






main = generate (rBinOp 10)
