import AST
import Test.QuickCheck
import System.Random

rBinOp :: Integer -> Gen BinaryOperation
rBinOp seed = variant seed $ chooseEnum (Add,Copysign)

rUnOp :: Integer -> Gen UnaryOperation
rUnOp seed = variant seed $ chooseEnum (Abs,Nearest)

rRelOp :: Integer -> Gen RelationalOperation
rRelOp seed = variant seed $ chooseEnum (Eq,Ge)

rConst :: Int -> Gen Expression
rConst seed = return $ Const d
  where
    d = fst $ random (mkStdGen seed)

rParam :: Int -> Int -> Gen Expression
rParam m seed = return $ Param i
  where
    i = fst $ randomR (0, m-1) (mkStdGen seed)

-- GROW generator
-- first argument : seed,
-- second argument : maxdepth,
-- third argument : amount of parameters
grow :: Int -> Int -> Int -> Gen Expression
grow seed 0 nrParam = oneof [(rConst seed), (rParam seed nrParam)]
