import AST
import Test.QuickCheck
import System.Random

rBinOp :: Int -> Gen BinaryOperation
rBinOp seed = variant seed $ chooseEnum (Add,Copysign)

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rBinExpr :: Int -> Gen Expression -> Gen Expression -> Gen Expression
rBinExpr seed gExpr1 gExpr2 = do
  op <- rBinOp seed
  e1 <- gExpr1
  e2 <- gExpr2
  return $ BinOp op e1 e2

rUnOp :: Int -> Gen UnaryOperation
rUnOp seed = variant seed $ chooseEnum (Abs,Nearest)

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rUnExpr :: Int -> Gen Expression -> Gen Expression
rUnExpr seed gExpr = do
  op <- rUnOp seed
  e <- gExpr
  return $ UnOp op e

rRelOp :: Int -> Gen RelationalOperation
rRelOp seed = variant seed $ chooseEnum (Eq,Ge)

-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
-- fourth argument : initialization method
rRelExpr :: Int -> Gen Expression -> Gen Expression -> Gen Expression
rRelExpr seed gExpr1 gExpr2 = do
  op <- rRelOp seed
  e1 <- gExpr1
  e2 <- gExpr2
  return $ RelOp op e1 e2

rConst :: Int -> Gen Expression
rConst seed = return $ Const $ fst $ random (mkStdGen seed)

rParam :: Int -> Int -> Gen Expression
rParam m seed = return $ Param $ fst $ randomR (0, m-1) (mkStdGen seed)

-- GROWONE generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
growOne :: Int -> Int -> Int -> Gen Expression
growOne seed 0 nrParam = oneof [(rConst seed), (rParam seed nrParam)]
growOne seed d nrParam = frequency [
  (1, rConst seed),
  (1, rParam seed nrParam),
  (7, rBinExpr seed (growOne seed (d-1) nrParam) (growOne seed (d-1) nrParam)),
  (7, rUnExpr seed (growOne seed (d-1) nrParam)),
  (7, rRelExpr seed (growOne seed (d-1) nrParam) (growOne seed (d-1) nrParam))
  ]

-- FULLONE generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
fullOne :: Int -> Int -> Int -> Gen Expression
fullOne seed 0 nrParam = oneof [(rConst seed), (rParam seed nrParam)]
fullOne seed d nrParam = frequency [
  (7, rBinExpr seed (growOne seed (d-1) nrParam) (growOne seed (d-1) nrParam)),
  (7, rUnExpr seed (growOne seed (d-1) nrParam)),
  (7, rRelExpr seed (growOne seed (d-1) nrParam) (growOne seed (d-1) nrParam))
  ]

-- list of initializations generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters,
-- fourth argument : amount of generators
generateInitList :: Int -> Int -> Int -> Int -> (Int -> Int -> Int -> Gen Expression) -> [Gen Expression]
generateInitList _ _ _ 0 _ = []
generateInitList seed d nrParam n gen = [gen seed d nrParam] ++ generateInitList seed d nrParam (n-1) gen
