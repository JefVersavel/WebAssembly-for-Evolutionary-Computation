import AST
import Test.QuickCheck
import Test.QuickCheck.Random
import Seeding
import Control.Monad.Reader

-- was gonna use chooseEnum instead of elements but it is not available anymore apparantly, weird?
rBinOp :: Gen BinaryOperation
rBinOp = elements [Add, Sub, Mul, Div, Min, Max, Copysign]

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rBinExpr :: Gen Expression -> Gen Expression -> Gen Expression
rBinExpr gExpr1 gExpr2 = do
  op <- rBinOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ BinOp op e1 e2

rUnOp :: Gen UnaryOperation
rUnOp = elements [Abs, Neg, Sqrt, Ceil, Floor, Trunc, Nearest]

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rUnExpr :: Gen Expression -> Gen Expression
rUnExpr gExpr = do
  op <- rUnOp
  e <- gExpr
  return $ UnOp op e

rRelOp :: Gen RelationalOperation
rRelOp = elements [Eq, Ne, Lt, Gt, Le, Ge]

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rRelExpr :: Gen Expression -> Gen Expression -> Gen Expression
rRelExpr gExpr1 gExpr2 = do
  op <- rRelOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ RelOp op e1 e2

rConst :: Gen Expression
rConst = Const <$> arbitrary

rParam :: Int -> Gen Expression
rParam m = Param <$> elements [0, 1 .. (m-1)]

-- GROWONE generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
growOne :: Int -> Int -> Reader (QCGen, Int) (Gen Expression)
growOne 0 nrParam = do 
  return $ oneof [rConst, (rParam nrParam)]
growOne d nrParam = do
  (gen, maxD) <- ask
  genLeft <- growOne (d-1) nrParam
  genRight <- growOne (d-1) nrParam
  let nextGrowth = frequency [(1, rConst), (1, rParam nrParam), (7, rBinExpr genLeft genRight), (7, rUnExpr genLeft), (6, rRelExpr genLeft genRight)]
  if d == maxD 
    then return $ useSeed gen $ nextGrowth
    else return $ nextGrowth
  

-- FULLONE generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
fullOne :: Int -> Int -> Gen Expression
fullOne 0 nrParam = oneof [rConst, (rParam nrParam)]
fullOne d nrParam = frequency [
  (7, rBinExpr (fullOne (d-1) nrParam) (fullOne (d-1) nrParam)),
  (7, rUnExpr (fullOne (d-1) nrParam)),
  (6, rRelExpr (fullOne (d-1) nrParam) (fullOne (d-1) nrParam))
  ]

-- list of initializations generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters,
-- fourth argument : amount of generators
-- fifth argument : specic initialization method (grow or full)
generateInitList :: Int -> Int -> Int -> (Int -> Int -> Gen Expression) -> [Gen Expression]
generateInitList _ _ 0 _ = []
generateInitList d nrParam n gen = [gen d nrParam] ++ generateInitList d nrParam (n-1) gen

-- executes the ramped half and half method for population initialization
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters,
-- fourth argument : amount of generators
-- fifth argument : ratio of of grow vs full
rampedHalfNHalf :: Int -> Int -> Int -> Float -> [Gen Expression]
rampedHalfNHalf d nrParam n ratio
  | ratio <=1 && ratio >= 0 = fullList
  | otherwise = error "Please provide a ratio between 0 and 1."
  where
    growN = floor $ ratio * fromIntegral n
    fullN = floor $ (1 - ratio) * fromIntegral n
    -- growList = generateInitList d nrParam growN growOne
    fullList = generateInitList d nrParam fullN fullOne


main = generate $ runReader (growOne 5 10) (mkQCGen 100, 5)