import AST
import Test.QuickCheck
import Test.QuickCheck.Random
import Seeding
import Control.Monad.Reader

-- was gonna use chooseEnum instead of elements but it is not available anymore apparantly, weird?
rBinOp :: Gen BinaryOperation
rBinOp = elements [Add, Sub, Mul, Div, Min, Max, Copysign]

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
-- second argument : maxdepth,
-- third argument : amount of parameters
growOne :: Int -> Int -> Gen Expression
growOne 0 nrParam = oneof [rConst, (rParam nrParam)]
growOne d nrParam = frequency [
  (1, rConst),
  (1, rParam nrParam),
  (7, rBinExpr (growOne (d-1) nrParam) (growOne (d-1) nrParam)),
  (7, rUnExpr (growOne (d-1) nrParam)),
  (7, rRelExpr (growOne (d-1) nrParam) (growOne (d-1) nrParam))
  ]

growOneInit :: QCGen -> Int -> Int -> Gen Expression
growOneInit gen d nrParam = useSeed gen $ growOne d nrParam
  

-- FULLONE generator
-- second argument : maxdepth,
-- third argument : amount of parameters
fullOne :: Int -> Int -> Gen Expression
fullOne 0 nrParam = oneof [rConst, (rParam nrParam)]
fullOne d nrParam = frequency [
  (7, rBinExpr (fullOne (d-1) nrParam) (fullOne (d-1) nrParam)),
  (7, rUnExpr (fullOne (d-1) nrParam)),
  (6, rRelExpr (fullOne (d-1) nrParam) (fullOne (d-1) nrParam))
  ]

fullOneInit :: QCGen -> Int -> Int -> Gen Expression
fullOneInit gen d nrParam = useSeed gen $ fullOne d nrParam

-- list of initializations generator
-- second argument : maxdepth,
-- third argument : amount of parameters,
-- fourth argument : amount of generators
-- fifth argument : specic initialization method (grow or full)
generateInitList :: [QCGen] -> Int -> Int -> (QCGen -> Int -> Int -> Gen Expression) -> [Gen Expression]
generateInitList gens d nrParam method = [method gen d nrParam | gen <- gens]

-- executes the ramped half and half method for population initialization
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters,
-- fourth argument : ratio of of grow vs full
rampedHalfNHalf :: QCGen -> Int -> Int -> Float -> Int -> [Gen Expression]
rampedHalfNHalf gen d nrParam ratio n
  | ratio <=1 && ratio >= 0 = growList ++ fullList
  | otherwise = error "Please provide a ratio between 0 and 1."
  where
    growN = floor $ ratio * fromIntegral n
    fullN = floor $ (1 - ratio) * fromIntegral n
    growSeeds = growSeedList gen growN
    fullSeeds = growSeedList (last growSeeds) fullN
    growList = generateInitList growSeeds d nrParam growOneInit
    fullList = generateInitList fullSeeds d nrParam fullOneInit

main = generate $ head [g| g <- rampedHalfNHalf (mkQCGen 99) 5 10 0.5 50]
